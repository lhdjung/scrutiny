// pkgdown places the copy button's tooltip to the left via Bootstrap 4
// attributes, which Bootstrap 5 ignores, so the tooltip opens above the button
// and covers the logo. This sets the default before pkgdown creates tooltips.
bootstrap.Tooltip.Default.placement = "left";
