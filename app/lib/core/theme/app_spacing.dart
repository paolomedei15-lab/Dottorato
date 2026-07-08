import 'package:flutter/widgets.dart';

/// Spacing, radius and sizing tokens on a 4-pt grid.
///
/// Field-first defaults: generous spacing and large minimum touch targets so
/// the app is usable with gloves and at arm's length in bright light.
abstract final class AppSpacing {
  static const double xs = 4;
  static const double sm = 8;
  static const double md = 16;
  static const double lg = 24;
  static const double xl = 32;
  static const double xxl = 48;

  /// Minimum interactive target — larger than Material's 48-dp default for
  /// gloved, outdoor use.
  static const double minTouchTarget = 56;

  /// Standard page padding.
  static const EdgeInsets pagePadding = EdgeInsets.all(md);

  /// Gap widgets for readable column/row layouts.
  static const SizedBox gapXs = SizedBox(height: xs, width: xs);
  static const SizedBox gapSm = SizedBox(height: sm, width: sm);
  static const SizedBox gapMd = SizedBox(height: md, width: md);
  static const SizedBox gapLg = SizedBox(height: lg, width: lg);
}

/// Corner-radius tokens.
abstract final class AppRadius {
  static const double sm = 8;
  static const double md = 16;
  static const double lg = 24;
  static const double pill = 999;
}
