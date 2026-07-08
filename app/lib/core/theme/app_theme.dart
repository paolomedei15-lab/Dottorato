import 'package:flutter/material.dart';

import 'app_colors.dart';
import 'app_spacing.dart';
import 'app_typography.dart';
import 'health_colors.dart';

/// Builds the app's Material 3 themes.
///
/// Three flavors share one design language:
///  • [light]        — standard indoor light theme.
///  • [lightOutdoor] — high-contrast variant for direct sunlight (bolder text,
///                     saturated health colors, stronger elevation).
///  • [dark]         — low-light / battery-friendly dark theme.
abstract final class AppTheme {
  static ThemeData light() => _build(
        brightness: Brightness.light,
        healthColors: HealthColors.standard,
      );

  static ThemeData lightOutdoor() => _build(
        brightness: Brightness.light,
        healthColors: HealthColors.outdoor,
        outdoor: true,
      );

  static ThemeData dark() => _build(
        brightness: Brightness.dark,
        healthColors: HealthColors.outdoor,
      );

  static ThemeData _build({
    required Brightness brightness,
    required HealthColors healthColors,
    bool outdoor = false,
  }) {
    final scheme = ColorScheme.fromSeed(
      seedColor: AppColors.brandSeed,
      brightness: brightness,
      // The outdoor theme leans on higher contrast between surfaces.
      contrastLevel: outdoor ? 0.5 : 0.0,
    );

    final textTheme = AppTypography.textThemeFor(scheme);

    return ThemeData(
      useMaterial3: true,
      colorScheme: scheme,
      // Bundled font (see pubspec) so text renders without a network fetch.
      fontFamily: 'Roboto',
      textTheme: textTheme,
      scaffoldBackgroundColor: scheme.surface,
      extensions: <ThemeExtension<dynamic>>[healthColors],
      appBarTheme: AppBarTheme(
        centerTitle: false,
        scrolledUnderElevation: outdoor ? 3 : 1,
        backgroundColor: scheme.surface,
        foregroundColor: scheme.onSurface,
        titleTextStyle: textTheme.titleLarge?.copyWith(
          color: scheme.onSurface,
        ),
      ),
      cardTheme: CardThemeData(
        clipBehavior: Clip.antiAlias,
        elevation: outdoor ? 2 : 1,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(AppRadius.md),
        ),
      ),
      // Large, field-friendly buttons everywhere.
      filledButtonTheme: FilledButtonThemeData(
        style: FilledButton.styleFrom(
          minimumSize: const Size(0, AppSpacing.minTouchTarget),
          textStyle: textTheme.labelLarge,
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(AppRadius.md),
          ),
        ),
      ),
      outlinedButtonTheme: OutlinedButtonThemeData(
        style: OutlinedButton.styleFrom(
          minimumSize: const Size(0, AppSpacing.minTouchTarget),
          textStyle: textTheme.labelLarge,
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(AppRadius.md),
          ),
        ),
      ),
      chipTheme: ChipThemeData(
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(AppRadius.pill),
        ),
      ),
      inputDecorationTheme: InputDecorationTheme(
        filled: true,
        border: OutlineInputBorder(
          borderRadius: BorderRadius.circular(AppRadius.md),
        ),
      ),
    );
  }
}
