import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

import '../../core/theme/app_spacing.dart';
import '../../core/theme/theme_controller.dart';
import '../../core/widgets/health_status_chip.dart';
import '../../data/mock/mock_data.dart';
import '../../domain/models/enums.dart';

/// Step 1 deliverable: a living style guide.
///
/// It exists so we can SEE and approve the design system — brand color, the
/// three theme modes (incl. the outdoor high-contrast mode), health tokens,
/// typography and a sample cow card — before building real screens. It will be
/// removed once the app shell lands in Step 2.
class DesignSystemScreen extends ConsumerWidget {
  const DesignSystemScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final mode = ref.watch(themeControllerProvider);
    final theme = Theme.of(context);

    return Scaffold(
      appBar: AppBar(
        title: const Text("Farmer's Wingman"),
        bottom: PreferredSize(
          preferredSize: const Size.fromHeight(64),
          child: Padding(
            padding: const EdgeInsets.fromLTRB(
                AppSpacing.md, 0, AppSpacing.md, AppSpacing.sm),
            child: _ThemeModeSelector(
              current: mode,
              onChanged: (m) =>
                  ref.read(themeControllerProvider.notifier).set(m),
            ),
          ),
        ),
      ),
      body: ListView(
        padding: AppSpacing.pagePadding,
        children: [
          const _SectionHeader('Design system — Step 1 foundation'),
          Text(
            'Toggle the modes above to preview the outdoor high-contrast '
            'theme. Everything below is a reusable building block.',
            style: theme.textTheme.bodyMedium,
          ),
          AppSpacing.gapLg,

          const _SectionHeader('Health status'),
          const Wrap(
            spacing: AppSpacing.sm,
            runSpacing: AppSpacing.sm,
            children: [
              HealthStatusChip(status: HealthStatus.healthy),
              HealthStatusChip(status: HealthStatus.warning),
              HealthStatusChip(status: HealthStatus.highRisk),
              HealthStatusChip(status: HealthStatus.unknown),
            ],
          ),
          AppSpacing.gapLg,

          const _SectionHeader('Sample cow card'),
          _CowPreviewCard(),
          AppSpacing.gapLg,

          const _SectionHeader('Quick stat tiles'),
          Row(
            children: [
              Expanded(
                child: _StatTile(
                  value: '${MockData.cows.length}',
                  label: 'Total cows',
                  icon: Icons.pets_rounded,
                ),
              ),
              AppSpacing.gapSm,
              Expanded(
                child: _StatTile(
                  value: '${MockData.alerts.length}',
                  label: 'Open alerts',
                  icon: Icons.notifications_active_rounded,
                ),
              ),
            ],
          ),
          AppSpacing.gapLg,

          const _SectionHeader('Typography'),
          Text('Headline medium', style: theme.textTheme.headlineMedium),
          Text('Title large', style: theme.textTheme.titleLarge),
          Text('Body large — readable at arm’s length in bright sunlight.',
              style: theme.textTheme.bodyLarge),
          Text('Body medium — secondary information and captions.',
              style: theme.textTheme.bodyMedium),
          AppSpacing.gapLg,

          const _SectionHeader('Buttons (56 dp, glove-friendly)'),
          Wrap(
            spacing: AppSpacing.sm,
            runSpacing: AppSpacing.sm,
            children: [
              FilledButton(onPressed: () {}, child: const Text('Primary')),
              OutlinedButton(onPressed: () {}, child: const Text('Secondary')),
            ],
          ),
          AppSpacing.gapLg,
        ],
      ),
    );
  }
}

class _ThemeModeSelector extends StatelessWidget {
  const _ThemeModeSelector({required this.current, required this.onChanged});

  final AppThemeMode current;
  final ValueChanged<AppThemeMode> onChanged;

  @override
  Widget build(BuildContext context) {
    return SegmentedButton<AppThemeMode>(
      segments: [
        for (final m in AppThemeMode.values)
          ButtonSegment(value: m, label: Text(m.label), icon: Icon(m.icon)),
      ],
      selected: {current},
      showSelectedIcon: false,
      onSelectionChanged: (s) => onChanged(s.first),
    );
  }
}

class _CowPreviewCard extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    final cow = MockData.cows.firstWhere(
      (c) => c.status == HealthStatus.warning,
      orElse: () => MockData.cows.first,
    );
    final theme = Theme.of(context);
    final now = DateTime(2026, 7, 8);

    return Card(
      child: Padding(
        padding: const EdgeInsets.all(AppSpacing.md),
        child: Row(
          children: [
            CircleAvatar(
              radius: 28,
              backgroundColor: theme.colorScheme.secondaryContainer,
              child: Icon(Icons.pets_rounded,
                  color: theme.colorScheme.onSecondaryContainer),
            ),
            AppSpacing.gapMd,
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(cow.displayName, style: theme.textTheme.titleMedium),
                  Text(
                    '${cow.breed} · Tag ${cow.tagNumber} · '
                    '${(cow.ageInMonths(now) / 12).toStringAsFixed(1)} yr',
                    style: theme.textTheme.bodyMedium,
                  ),
                  AppSpacing.gapXs,
                  Text('${cow.activity.label} · '
                      '${(cow.diseaseProbability * 100).round()}% risk · '
                      '${(cow.confidence * 100).round()}% confidence',
                      style: theme.textTheme.bodyMedium),
                ],
              ),
            ),
            AppSpacing.gapSm,
            HealthStatusChip(status: cow.status, dense: true),
          ],
        ),
      ),
    );
  }
}

class _StatTile extends StatelessWidget {
  const _StatTile({
    required this.value,
    required this.label,
    required this.icon,
  });

  final String value;
  final String label;
  final IconData icon;

  @override
  Widget build(BuildContext context) {
    final theme = Theme.of(context);
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(AppSpacing.md),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Icon(icon, color: theme.colorScheme.primary),
            AppSpacing.gapSm,
            Text(value, style: theme.textTheme.headlineMedium),
            Text(label, style: theme.textTheme.bodyMedium),
          ],
        ),
      ),
    );
  }
}

class _SectionHeader extends StatelessWidget {
  const _SectionHeader(this.text);
  final String text;

  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.only(bottom: AppSpacing.sm),
      child: Text(
        text.toUpperCase(),
        style: Theme.of(context).textTheme.labelLarge?.copyWith(
              color: Theme.of(context).colorScheme.primary,
              letterSpacing: 0.8,
            ),
      ),
    );
  }
}
