import 'package:flutter/material.dart';

import '../../domain/models/enums.dart';
import '../theme/app_spacing.dart';
import '../theme/health_colors.dart';

/// The canonical way to show a cow's health status.
///
/// Reused across dashboard, lists, profile and map callouts. Encodes status
/// with color **and** an icon **and** a text label — never color alone — so it
/// stays legible in sunlight and for color-blind users.
class HealthStatusChip extends StatelessWidget {
  const HealthStatusChip({
    super.key,
    required this.status,
    this.dense = false,
  });

  final HealthStatus status;

  /// Compact form (icon + shorter padding) for tight list rows.
  final bool dense;

  static IconData iconFor(HealthStatus status) => switch (status) {
        HealthStatus.healthy => Icons.check_circle_rounded,
        HealthStatus.warning => Icons.warning_amber_rounded,
        HealthStatus.highRisk => Icons.report_rounded,
        HealthStatus.unknown => Icons.help_outline_rounded,
      };

  @override
  Widget build(BuildContext context) {
    final health = Theme.of(context).extension<HealthColors>()!;
    final pair = health.pairFor(status);
    final textStyle = Theme.of(context).textTheme.labelLarge;

    return Container(
      padding: EdgeInsets.symmetric(
        horizontal: dense ? AppSpacing.sm : AppSpacing.md,
        vertical: dense ? AppSpacing.xs : AppSpacing.sm,
      ),
      decoration: BoxDecoration(
        color: pair.fill,
        borderRadius: BorderRadius.circular(AppRadius.pill),
      ),
      child: Row(
        mainAxisSize: MainAxisSize.min,
        children: [
          Icon(iconFor(status), size: dense ? 16 : 18, color: pair.on),
          const SizedBox(width: AppSpacing.xs),
          Text(
            status.label,
            style: textStyle?.copyWith(color: pair.on),
          ),
        ],
      ),
    );
  }
}

/// A small solid dot for the health status — used as a map marker glyph or a
/// leading indicator. Pair it with a label or the [HealthStatusChip] where the
/// status must be understood on its own.
class HealthStatusDot extends StatelessWidget {
  const HealthStatusDot({super.key, required this.status, this.size = 16});

  final HealthStatus status;
  final double size;

  @override
  Widget build(BuildContext context) {
    final health = Theme.of(context).extension<HealthColors>()!;
    final pair = health.pairFor(status);
    return Container(
      width: size,
      height: size,
      decoration: BoxDecoration(
        color: pair.fill,
        shape: BoxShape.circle,
        border: Border.all(color: pair.on.withValues(alpha: 0.6), width: 1.5),
      ),
    );
  }
}
