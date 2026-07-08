import 'enums.dart';
import 'geo_point.dart';

/// A single AI detection for a cow at a point in time.
///
/// This is the time-series record that powers historical trends on the vet's
/// profile screen. One drone flight produces many observations.
class Observation {
  const Observation({
    required this.id,
    required this.cowId,
    required this.timestamp,
    required this.location,
    required this.activity,
    required this.bodyConditionScore,
    required this.diseaseProbability,
    required this.confidence,
    this.flightId,
    this.imageUrls = const [],
  });

  final String id;
  final String cowId;
  final DateTime timestamp;
  final GeoPoint location;
  final CowActivity activity;

  /// Body condition score, conventionally 1–5 (thin → fat).
  final double bodyConditionScore;

  /// AI-estimated probability of disease, 0.0–1.0.
  final double diseaseProbability;

  /// AI confidence in this observation, 0.0–1.0.
  final double confidence;

  /// The drone flight this observation came from, if known.
  final String? flightId;

  /// Drone image URLs captured for this observation.
  final List<String> imageUrls;

  /// Health bucket derived from the disease probability. Central rule so the
  /// whole app classifies consistently. (Thresholds are placeholders until the
  /// AI team confirms the real cut-offs.)
  HealthStatus get derivedStatus {
    if (diseaseProbability >= 0.66) return HealthStatus.highRisk;
    if (diseaseProbability >= 0.33) return HealthStatus.warning;
    return HealthStatus.healthy;
  }
}
