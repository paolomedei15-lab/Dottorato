import 'geo_point.dart';

/// A completed drone monitoring flight. Flown by the external drone system; the
/// app only displays its summary and the observations it produced.
class DroneFlight {
  const DroneFlight({
    required this.id,
    required this.farmId,
    required this.startedAt,
    required this.endedAt,
    required this.cowsObserved,
    required this.pastureCoveragePercent,
    required this.path,
  });

  final String id;
  final String farmId;
  final DateTime startedAt;
  final DateTime endedAt;
  final int cowsObserved;

  /// Estimated % of pasture area covered by this flight.
  final double pastureCoveragePercent;

  /// Ordered GPS breadcrumbs of the flight path (for map replay).
  final List<GeoPoint> path;

  Duration get duration => endedAt.difference(startedAt);
}
