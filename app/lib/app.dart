import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import 'core/theme/theme_controller.dart';
import 'features/foundation/design_system_screen.dart';

/// Root widget. Watches the theme controller so switching between Standard /
/// Outdoor / Dark rebuilds the whole app.
///
/// For Step 1 the home is a design-system showcase. From Step 2 this is
/// replaced by the go_router-driven adaptive app shell.
class FarmersWingmanApp extends ConsumerWidget {
  const FarmersWingmanApp({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final mode = ref.watch(themeControllerProvider);
    return MaterialApp(
      title: "Farmer's Wingman",
      debugShowCheckedModeBanner: false,
      theme: mode.themeData,
      home: const DesignSystemScreen(),
    );
  }
}
