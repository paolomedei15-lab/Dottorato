import 'enums.dart';

/// A health alert raised when an observation crosses a risk threshold.
///
/// Alerts flow from the AI system into the app. The vet's notes/actions flow
/// back the other way (added later).
class HealthAlert {
  const HealthAlert({
    required this.id,
    required this.cowId,
    required this.cowLabel,
    required this.disease,
    required this.riskLevel,
    required this.confidence,
    required this.priority,
    required this.reason,
    required this.recommendedAction,
    required this.raisedAt,
    this.acknowledged = false,
  });

  final String id;
  final String cowId;

  /// Denormalised cow label (name or tag) so alert lists render without a join.
  final String cowLabel;

  final String disease;
  final HealthStatus riskLevel;
  final double confidence; // 0..1
  final AlertPriority priority;

  /// Plain-language explanation of why the AI flagged this.
  final String reason;

  /// What the farmer/vet should do next.
  final String recommendedAction;

  final DateTime raisedAt;
  final bool acknowledged;
}
