import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import 'package:farmers_wingman/app.dart';

void main() {
  testWidgets('App boots and shows the design-system showcase',
      (WidgetTester tester) async {
    await tester.pumpWidget(
      const ProviderScope(child: FarmersWingmanApp()),
    );

    // Title in the app bar and at least one health chip should render.
    expect(find.text("Farmer's Wingman"), findsOneWidget);
    expect(find.text('Healthy'), findsWidgets);
  });
}
