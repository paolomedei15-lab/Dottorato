import 'package:flutter/material.dart';

import '../../domain/models/enums.dart';
import 'app_colors.dart';

/// Resolved fill + text color for one health status.
typedef HealthColorPair = ({Color fill, Color on});

/// Theme extension holding the semantic health-status colors, so widgets
/// resolve them from the active theme. Two variants ship: [standard] (soft
/// tints, for indoor use) and [outdoor] (saturated, white text, for direct
/// sunlight). The active variant is swapped by the theme, so a single
/// `Theme.of(context).extension<HealthColors>()` call works everywhere.
@immutable
class HealthColors extends ThemeExtension<HealthColors> {
  const HealthColors({
    required this.healthy,
    required this.onHealthy,
    required this.warning,
    required this.onWarning,
    required this.highRisk,
    required this.onHighRisk,
    required this.unknown,
    required this.onUnknown,
  });

  final Color healthy;
  final Color onHealthy;
  final Color warning;
  final Color onWarning;
  final Color highRisk;
  final Color onHighRisk;
  final Color unknown;
  final Color onUnknown;

  /// Standard (indoor): soft container fills with dark text.
  static const HealthColors standard = HealthColors(
    healthy: AppColors.healthyContainer,
    onHealthy: Color(0xFF0B3D14),
    warning: AppColors.warningContainer,
    onWarning: Color(0xFF3F2D00),
    highRisk: AppColors.highRiskContainer,
    onHighRisk: Color(0xFF5C0000),
    unknown: AppColors.unknownContainer,
    onUnknown: Color(0xFF263238),
  );

  /// Outdoor / high-contrast: saturated fills with white text for glare.
  static const HealthColors outdoor = HealthColors(
    healthy: AppColors.healthy,
    onHealthy: Colors.white,
    warning: AppColors.warning,
    onWarning: Colors.white,
    highRisk: AppColors.highRisk,
    onHighRisk: Colors.white,
    unknown: AppColors.unknown,
    onUnknown: Colors.white,
  );

  /// Resolve the fill + text pair for a given [HealthStatus].
  HealthColorPair pairFor(HealthStatus status) => switch (status) {
        HealthStatus.healthy => (fill: healthy, on: onHealthy),
        HealthStatus.warning => (fill: warning, on: onWarning),
        HealthStatus.highRisk => (fill: highRisk, on: onHighRisk),
        HealthStatus.unknown => (fill: unknown, on: onUnknown),
      };

  @override
  HealthColors copyWith({
    Color? healthy,
    Color? onHealthy,
    Color? warning,
    Color? onWarning,
    Color? highRisk,
    Color? onHighRisk,
    Color? unknown,
    Color? onUnknown,
  }) {
    return HealthColors(
      healthy: healthy ?? this.healthy,
      onHealthy: onHealthy ?? this.onHealthy,
      warning: warning ?? this.warning,
      onWarning: onWarning ?? this.onWarning,
      highRisk: highRisk ?? this.highRisk,
      onHighRisk: onHighRisk ?? this.onHighRisk,
      unknown: unknown ?? this.unknown,
      onUnknown: onUnknown ?? this.onUnknown,
    );
  }

  @override
  HealthColors lerp(HealthColors? other, double t) {
    if (other is! HealthColors) return this;
    return HealthColors(
      healthy: Color.lerp(healthy, other.healthy, t)!,
      onHealthy: Color.lerp(onHealthy, other.onHealthy, t)!,
      warning: Color.lerp(warning, other.warning, t)!,
      onWarning: Color.lerp(onWarning, other.onWarning, t)!,
      highRisk: Color.lerp(highRisk, other.highRisk, t)!,
      onHighRisk: Color.lerp(onHighRisk, other.onHighRisk, t)!,
      unknown: Color.lerp(unknown, other.unknown, t)!,
      onUnknown: Color.lerp(onUnknown, other.onUnknown, t)!,
    );
  }
}
