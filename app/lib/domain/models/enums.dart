// Pure-Dart domain enums (no Flutter imports — the domain layer stays
// framework-agnostic so it can be unit-tested and, later, reused).

/// The at-a-glance health signal for a cow, derived by the external AI system.
///
/// Drives the green / amber / red markers across the app. In the UI it is
/// ALWAYS paired with an icon or text label — never color alone — for sunlight
/// legibility and color-blind accessibility.
enum HealthStatus { healthy, warning, highRisk, unknown }

extension HealthStatusX on HealthStatus {
  String get label => switch (this) {
        HealthStatus.healthy => 'Healthy',
        HealthStatus.warning => 'Warning',
        HealthStatus.highRisk => 'High risk',
        HealthStatus.unknown => 'Unknown',
      };
}

/// One user account can hold one or more roles (see [Membership]); the role
/// unlocks screens and permissions rather than defining a separate app.
enum UserRole { farmer, veterinarian, admin }

extension UserRoleX on UserRole {
  String get label => switch (this) {
        UserRole.farmer => 'Farmer',
        UserRole.veterinarian => 'Veterinarian',
        UserRole.admin => 'Administrator',
      };
}

/// Behaviour classification reported by the AI for a single observation.
enum CowActivity {
  grazing,
  ruminating,
  resting,
  walking,
  drinking,
  isolated,
  unknown,
}

extension CowActivityX on CowActivity {
  String get label => switch (this) {
        CowActivity.grazing => 'Grazing',
        CowActivity.ruminating => 'Ruminating',
        CowActivity.resting => 'Resting',
        CowActivity.walking => 'Walking',
        CowActivity.drinking => 'Drinking',
        CowActivity.isolated => 'Isolated from herd',
        CowActivity.unknown => 'Unknown',
      };
}

/// Urgency of an alert. Used for sorting and for the alert accent color.
enum AlertPriority { low, medium, high, critical }

extension AlertPriorityX on AlertPriority {
  String get label => switch (this) {
        AlertPriority.low => 'Low',
        AlertPriority.medium => 'Medium',
        AlertPriority.high => 'High',
        AlertPriority.critical => 'Critical',
      };

  /// Higher = more urgent. Handy for sorting alert lists.
  int get weight => switch (this) {
        AlertPriority.low => 0,
        AlertPriority.medium => 1,
        AlertPriority.high => 2,
        AlertPriority.critical => 3,
      };
}
