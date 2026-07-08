import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import 'app.dart';

void main() {
  runApp(
    // ProviderScope makes Riverpod state available to the whole tree.
    const ProviderScope(child: FarmersWingmanApp()),
  );
}
