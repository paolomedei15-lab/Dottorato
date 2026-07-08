import 'enums.dart';
import 'geo_point.dart';

/// An individual animal. The [status], [diseaseProbability] etc. reflect the
/// most recent [Observation] and are denormalised onto the cow for fast list
/// and map rendering.
class Cow {
  const Cow({
    required this.id,
    required this.tagNumber,
    required this.breed,
    required this.birthDate,
    required this.farmId,
    required this.status,
    required this.diseaseProbability,
    required this.confidence,
    required this.activity,
    required this.lastLocation,
    required this.lastSeen,
    this.name,
    this.photoUrl,
  });

  final String id;

  /// Ear-tag / official identifier.
  final String tagNumber;

  /// Optional friendly name farmers give their animals.
  final String? name;

  final String breed;
  final DateTime birthDate;
  final String farmId;
  final String? photoUrl;

  // --- Latest snapshot (from the most recent observation) ---
  final HealthStatus status;
  final double diseaseProbability; // 0..1
  final double confidence; // 0..1
  final CowActivity activity;
  final GeoPoint lastLocation;
  final DateTime lastSeen;

  /// Display label: friendly name if present, otherwise the tag number.
  String get displayName => name ?? 'Tag $tagNumber';

  /// Whole-month/year age derived from [birthDate].
  int ageInMonths(DateTime now) =>
      (now.year - birthDate.year) * 12 + (now.month - birthDate.month);
}
