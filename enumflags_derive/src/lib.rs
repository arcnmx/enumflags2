#![recursion_limit = "2048"]
extern crate proc_macro;
#[macro_use]
extern crate quote;

use syn::{Data, Ident, DeriveInput, DataEnum, spanned::Spanned};
use proc_macro2::TokenStream;
use proc_macro2::Span;
use std::convert::From;

/// Shorthand for a quoted `compile_error!`.
macro_rules! error {
    ($span:expr => $($x:tt)*) => {
        quote_spanned!($span => compile_error!($($x)*);)
    };
    ($($x:tt)*) => {
        quote!(compile_error!($($x)*);)
    };
}

#[proc_macro_derive(BitFlags_internal)]
pub fn derive_enum_flags(input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let ast: DeriveInput = syn::parse(input).unwrap();

    match ast.data {
        Data::Enum(ref data) => {
            gen_enumflags(&ast.ident, &ast, data)
                .unwrap_or_else(|err| err)
                .into()
        }
        _ => error!("BitFlags can only be derived on enums").into(),
    }
}

#[derive(Debug)]
enum EvaluationError {
    LiteralOutOfRange(Span),
    UnsupportedOperation(Span),
}

impl From<EvaluationError> for TokenStream {
    fn from(why: EvaluationError) -> TokenStream {
        use crate::EvaluationError::*;

        match why {
            LiteralOutOfRange(span) => {
                error!(span => "Integer literal out of range")
            }
            UnsupportedOperation(span) => {
                error!(span => "This kind of discriminant expression is \
                        not supported.\n\
                        hint: Enable the \"not_literal\" feature to \
                        use a workaround.\n\
                        note: This is not enabled by default due to the \
                        high potential for confusing error messages \
                        (see documentation).")
            }
        }
    }
}

/// Try to evaluate the expression given.
fn fold_expr(expr: &syn::Expr) -> Result<u64, EvaluationError> {
    use syn::Expr;
    use crate::EvaluationError::*;
    match expr {
        Expr::Lit(ref expr_lit) => {
            match expr_lit.lit {
                syn::Lit::Int(ref lit_int) => {
                    lit_int.base10_parse()
                        .map_err(|_| LiteralOutOfRange(expr.span()))
                }
                _ => Err(UnsupportedOperation(expr.span()))
            }
        },
        Expr::Binary(ref expr_binary) => {
            let l = fold_expr(&expr_binary.left)?;
            let r = fold_expr(&expr_binary.right)?;
            match &expr_binary.op {
                syn::BinOp::Shl(_) => Ok(l << r),
                _ => Err(UnsupportedOperation(expr_binary.span()))
            }
        }
        _ => Err(UnsupportedOperation(expr.span()))
    }
}

/// Given a list of attributes, find the `repr`, if any, and return the integer
/// type specified.
fn extract_repr(attrs: &[syn::Attribute])
    -> Result<Option<syn::Ident>, TokenStream>
{
    use syn::{Meta, NestedMeta};
    attrs.iter()
        .find_map(|attr| {
            match attr.parse_meta() {
                Err(why) => {
                    let error = format!("Couldn't parse attribute: {}", why);
                    Some(Err(error!(attr.span() => #error)))
                }
                Ok(Meta::List(ref meta)) if meta.path.is_ident("repr") => {
                    meta.nested.iter()
                        .find_map(|mi| match mi {
                            NestedMeta::Meta(Meta::Path(path)) => {
                                path.get_ident().cloned()
                                    .map(Ok)
                            }
                            _ => None
                        })
                }
                Ok(_) => None
            }
        })
        .transpose()
}

/// Returns Ok with deferred checks (not_literal), or Err with error!
fn verify_flag_values<'a>(
    // starts with underscore to silence warnings when not_literal
    // are disabled
    _type_name: &Ident,
    variants: impl Iterator<Item=&'a syn::Variant>
) -> Result<TokenStream, TokenStream> {
    #[cfg_attr(not(feature = "not_literal"), allow(unused_mut))]
    let mut deferred_checks: Vec<TokenStream> = vec![];
    for variant in variants {
        let discr = variant.discriminant.as_ref()
           .ok_or_else(|| error!(variant.span() =>
                         "Please add an explicit discriminant"))?;
        match fold_expr(&discr.1) {
            Ok(flag) => {
                if !flag.is_power_of_two() {
                    return Err(error!(variant.discriminant.as_ref()
                                      .unwrap().1.span() =>
                        "Flags must have exactly one set bit."));
                }
            }
            #[cfg(feature = "not_literal")]
            Err(EvaluationError::UnsupportedOperation(_)) => {
                let variant_name = &variant.ident;
                // TODO: Remove this madness when Debian ships a new compiler.
                let assertion_name = syn::Ident::new(
                    &format!("__enumflags_assertion_{}_{}",
                            _type_name, variant_name),
                    Span::call_site()); // call_site because def_site is unstable
                // adapted from static-assertions-rs by nvzqz (MIT/Apache-2.0)
                deferred_checks.push(quote_spanned!(variant.span() =>
                    #[allow(unknown_lints, eq_op)]
                    const #assertion_name: [(); 0 - !(
                        (#_type_name::#variant_name as u64).wrapping_sub(1) &
                        (#_type_name::#variant_name as u64) == 0 &&
                        (#_type_name::#variant_name as u64) != 0
                    ) as usize] = [];
                ));
            }
            Err(why) => return Err(why.into()),
        }
    }

    Ok(quote!(
        #(#deferred_checks)*
    ))
}

fn gen_enumflags(ident: &Ident, item: &DeriveInput, data: &DataEnum)
    -> Result<TokenStream, TokenStream>
{
    let span = Span::call_site();
    // for quote! interpolation
    let variants = data.variants.iter().map(|v| &v.ident);
    let variants_len = data.variants.len();
    let names = std::iter::repeat(&ident);
    let ty = extract_repr(&item.attrs)?
        .unwrap_or_else(|| Ident::new("usize", span));

    let deferred = verify_flag_values(ident, data.variants.iter())?;
    let std_path = quote_spanned!(span => ::enumflags2::_internal::core);
    let all = if variants_len == 0 {
        quote!(0)
    } else {
        let names = names.clone();
        let variants = variants.clone();
        quote!(#(#names::#variants as #ty)|*)
    };

    Ok(quote_spanned! {
        span => #deferred
            impl #std_path::ops::Not for #ident {
                type Output = <Self as ::enumflags2::BitFlag>::Flags;
                fn not(self) -> Self::Output {
                    use ::enumflags2::{BitFlagExt, EnumFlags};
                    self.into_flags().not()
                }
            }

            impl #std_path::ops::BitOr for #ident {
                type Output = <Self as ::enumflags2::BitFlag>::Flags;
                fn bitor(self, other: Self) -> Self::Output {
                    use ::enumflags2::{BitFlagExt, EnumFlags};
                    self.into_flags() | other
                }
            }

            impl #std_path::ops::BitAnd for #ident {
                type Output = <Self as ::enumflags2::BitFlag>::Flags;
                fn bitand(self, other: Self) -> Self::Output {
                    use ::enumflags2::{BitFlagExt, EnumFlags};
                    self.into_flags() & other
                }
            }

            impl #std_path::ops::BitXor for #ident {
                type Output = <Self as ::enumflags2::BitFlag>::Flags;
                fn bitxor(self, other: Self) -> Self::Output {
                    use ::enumflags2::{BitFlagExt, EnumFlags};
                    self.into_flags() ^ other
                }
            }

            unsafe impl ::enumflags2::repr::BitFlagRepr<#ident> for #ident {
                #[inline]
                fn into_repr(self) -> #ty {
                    self as #ty
                }

                #[inline]
                fn get_repr(&self) -> #ty {
                    *self as #ty
                }

                fn from_repr(bits: #ty) -> #std_path::result::Result<Self, #ty> {
                    use ::enumflags2::BitFlag;
                    use #std_path::result::Result;
                    if bits & !Self::ALL_BITS == 0 && bits.count_ones() == 1 {
                        // NOTE: avoid a match by relying on the invariant that
                        //       a flag may not contain more than one bit
                        Result::Ok(unsafe { Self::from_repr_unchecked(bits) })
                    } else {
                        Result::Err(bits)
                    }
                }

                unsafe fn from_repr_unchecked(bits: #ty) -> Self {
                    #std_path::mem::transmute(bits)
                }
            }

            impl ::enumflags2::BitFlag for #ident {
                type Type = #ty;
                type Flags = ::enumflags2::BitFlags<Self>;

                const ALL_BITS: Self::Type = #all;

                #[inline]
                fn flag_list() -> &'static [Self] {
                    const VARIANTS: [#ident; #variants_len] = [#(#names :: #variants),*];
                    &VARIANTS
                }

                #[inline]
                fn bitflags_type_name() -> &'static str {
                    concat!("BitFlags<", stringify!(#ident), ">")
                }
            }

            impl ::enumflags2::RawBitFlags for #ident {}
    })
}
