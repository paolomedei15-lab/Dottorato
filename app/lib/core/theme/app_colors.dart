import 'package:flutter/material.dart';

/// Raw, non-semantic color palette — the single source of truth for the theme.
///
/// Widgets should read colors from the [ColorScheme] or the [HealthColors]
/// theme extension, not from here directly.
abstract final class AppColors {
  /// Brand seed — a grounded agricultural teal-green. Deliberately darker and
  /// less saturated than the "healthy" status green so the brand and the
  /// health signal never read as the same color.
  static const Color brandSeed = Color(0xFF15706B);

  // --- Health status (vivid; ALWAYS paired with an icon/label in the UI) ---
  static const Color healthy = Color(0xFF2E7D32); // green 800
  static const Color warning = Color(0xFFF57F17); // amber 900
  static const Color highRisk = Color(0xFFC62828); // red 800
  static const Color unknown = Color(0xFF546E7A); // blue-grey 600

  // Soft container tints for the standard (indoor) theme.
  static const Color healthyContainer = Color(0xFFBFE7C1);
  static const Color warningContainer = Color(0xFFFFE39E);
  static const Color highRiskContainer = Color(0xFFFFCDD2);
  static const Color unknownContainer = Color(0xFFCFD8DC);
}
