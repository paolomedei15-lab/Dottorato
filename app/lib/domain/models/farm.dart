import 'geo_point.dart';

/// A farm/holding. The top of the ownership hierarchy: Farm → Cow → Observation.
class Farm {
  const Farm({
    required this.id,
    required this.name,
    required this.location,
    required this.areaHectares,
    this.region,
  });

  final String id;
  final String name;
  final GeoPoint location;
  final double areaHectares;
  final String? region;
}
