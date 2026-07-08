import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import 'app_theme.dart';

/// The three visual modes the user can pick. "Outdoor" is the high-contrast,
/// sunlight-legible variant — a first-class mode, not an afterthought.
enum AppThemeMode {
  standard('Standard', Icons.light_mode_outlined),
  outdoor('Outdoor', Icons.wb_sunny_rounded),
  dark('Dark', Icons.dark_mode_outlined);

  const AppThemeMode(this.label, this.icon);
  final String label;
  final IconData icon;

  ThemeData get themeData => switch (this) {
        AppThemeMode.standard => AppTheme.light(),
        AppThemeMode.outdoor => AppTheme.lightOutdoor(),
        AppThemeMode.dark => AppTheme.dark(),
      };
}

/// Holds the active theme mode. Later this will persist the choice to local
/// storage; for now it lives in memory.
class ThemeController extends Notifier<AppThemeMode> {
  @override
  AppThemeMode build() => AppThemeMode.standard;

  void set(AppThemeMode mode) => state = mode;
}

final themeControllerProvider =
    NotifierProvider<ThemeController, AppThemeMode>(ThemeController.new);
