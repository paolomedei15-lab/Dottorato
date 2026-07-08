import '../../domain/models/alert.dart';
import '../../domain/models/app_user.dart';
import '../../domain/models/cow.dart';
import '../../domain/models/drone_flight.dart';
import '../../domain/models/enums.dart';
import '../../domain/models/farm.dart';
import '../../domain/models/geo_point.dart';

/// In-memory sample data used to build and demo the UI before any backend is
/// wired in (Step 8). Everything here is fake but realistically shaped.
abstract final class MockData {
  static final DateTime _now = DateTime(2026, 7, 8, 9, 30);

  static const Farm farm = Farm(
    id: 'farm_1',
    name: 'Green Valley Ranch',
    location: GeoPoint(43.7696, 11.2558), // near Florence, IT
    areaHectares: 84,
    region: 'Tuscany',
  );

  static const AppUser demoFarmer = AppUser(
    id: 'user_farmer',
    fullName: 'Marco Rossi',
    email: 'marco@greenvalley.example',
    memberships: [Membership(farmId: 'farm_1', role: UserRole.farmer)],
  );

  static const AppUser demoVet = AppUser(
    id: 'user_vet',
    fullName: 'Dr. Elena Bianchi',
    email: 'elena.bianchi@vetcare.example',
    memberships: [
      Membership(farmId: 'farm_1', role: UserRole.veterinarian),
    ],
  );

  static final List<Cow> cows = [
    _cow('cow_1', '0421', 'Bella', 'Holstein Friesian', 38,
        HealthStatus.healthy, 0.08, 0.94, CowActivity.grazing, 0.0009, 0.0012),
    _cow('cow_2', '0387', 'Luna', 'Brown Swiss', 52, HealthStatus.warning,
        0.41, 0.81, CowActivity.isolated, -0.0011, 0.0018),
    _cow('cow_3', '0455', null, 'Holstein Friesian', 26, HealthStatus.highRisk,
        0.78, 0.88, CowActivity.resting, 0.0021, -0.0008),
    _cow('cow_4', '0402', 'Maggie', 'Jersey', 44, HealthStatus.healthy, 0.12,
        0.90, CowActivity.ruminating, -0.0006, 0.0022),
    _cow('cow_5', '0410', 'Daisy', 'Holstein Friesian', 31,
        HealthStatus.healthy, 0.05, 0.96, CowActivity.grazing, 0.0014, 0.0005),
    _cow('cow_6', '0398', 'Rosa', 'Chianina', 61, HealthStatus.warning, 0.36,
        0.72, CowActivity.walking, -0.0018, -0.0015),
  ];

  static final List<HealthAlert> alerts = [
    HealthAlert(
      id: 'alert_1',
      cowId: 'cow_3',
      cowLabel: 'Tag 0455',
      disease: 'Lameness (suspected hoof lesion)',
      riskLevel: HealthStatus.highRisk,
      confidence: 0.88,
      priority: AlertPriority.critical,
      reason:
          'Reduced movement and prolonged lying time over the last 2 flights, '
          'with an abnormal gait pattern detected.',
      recommendedAction:
          'Inspect hooves today and isolate for veterinary examination.',
      raisedAt: _now.subtract(const Duration(hours: 1, minutes: 20)),
    ),
    HealthAlert(
      id: 'alert_2',
      cowId: 'cow_2',
      cowLabel: 'Luna',
      disease: 'Possible early illness (isolation behaviour)',
      riskLevel: HealthStatus.warning,
      confidence: 0.81,
      priority: AlertPriority.high,
      reason:
          'Separated from the herd for a prolonged period and lower grazing '
          'activity than her 7-day baseline.',
      recommendedAction:
          'Observe closely at the next flight; check feed and water intake.',
      raisedAt: _now.subtract(const Duration(hours: 3)),
    ),
  ];

  static final List<DroneFlight> flights = [
    DroneFlight(
      id: 'flight_1',
      farmId: 'farm_1',
      startedAt: _now.subtract(const Duration(hours: 1, minutes: 45)),
      endedAt: _now.subtract(const Duration(hours: 1, minutes: 5)),
      cowsObserved: 6,
      pastureCoveragePercent: 92,
      path: const [
        GeoPoint(43.7696, 11.2558),
        GeoPoint(43.7705, 11.2571),
        GeoPoint(43.7712, 11.2549),
      ],
    ),
  ];

  // --- helpers ---

  static Cow _cow(
    String id,
    String tag,
    String? name,
    String breed,
    int ageMonths,
    HealthStatus status,
    double diseaseProb,
    double confidence,
    CowActivity activity,
    double dLat,
    double dLng,
  ) {
    return Cow(
      id: id,
      tagNumber: tag,
      name: name,
      breed: breed,
      birthDate: DateTime(_now.year, _now.month - ageMonths, _now.day),
      farmId: farm.id,
      status: status,
      diseaseProbability: diseaseProb,
      confidence: confidence,
      activity: activity,
      lastLocation: GeoPoint(
        farm.location.latitude + dLat,
        farm.location.longitude + dLng,
      ),
      lastSeen: _now.subtract(const Duration(minutes: 10)),
    );
  }
}
