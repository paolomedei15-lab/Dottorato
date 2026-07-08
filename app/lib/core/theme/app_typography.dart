import 'package:flutter/material.dart';

/// Typography for Farmer's Wingman.
///
/// We use the platform default (Roboto) rather than a network font package:
/// this app must work offline in the field, so we never depend on runtime font
/// downloads. Sizes are nudged up from the Material defaults for readability at
/// arm's length in bright sunlight. (A bundled font can be added later without
/// touching call sites.)
abstract final class AppTypography {
  static TextTheme textThemeFor(ColorScheme scheme) {
    final base = Typography.material2021(colorScheme: scheme).black;
    return base.copyWith(
      headlineMedium: base.headlineMedium?.copyWith(
        fontWeight: FontWeight.w700,
      ),
      titleLarge: base.titleLarge?.copyWith(fontWeight: FontWeight.w700),
      titleMedium: base.titleMedium?.copyWith(fontWeight: FontWeight.w600),
      // Slightly larger, comfortable body text for outdoor reading.
      bodyLarge: base.bodyLarge?.copyWith(fontSize: 17, height: 1.35),
      bodyMedium: base.bodyMedium?.copyWith(fontSize: 15, height: 1.35),
      labelLarge: base.labelLarge?.copyWith(
        fontSize: 15,
        fontWeight: FontWeight.w600,
      ),
    );
  }
}
