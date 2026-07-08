import 'enums.dart';

/// An authenticated user. Roles are held via [memberships] so one person can be
/// (say) a vet on several farms and an admin on none.
class AppUser {
  const AppUser({
    required this.id,
    required this.fullName,
    required this.email,
    required this.memberships,
    this.avatarUrl,
  });

  final String id;
  final String fullName;
  final String email;
  final String? avatarUrl;
  final List<Membership> memberships;

  /// All distinct roles this user holds across every farm.
  Set<UserRole> get roles => memberships.map((m) => m.role).toSet();

  bool hasRole(UserRole role) => roles.contains(role);
}

/// Links a user to a farm with a specific role. This is the unit that powers
/// permissions (a user may appear once per farm they belong to).
class Membership {
  const Membership({
    required this.farmId,
    required this.role,
  });

  final String farmId;
  final UserRole role;
}
