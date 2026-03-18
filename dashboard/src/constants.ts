import { MetricDef } from './types';

export const getQuintileRange = (value: number): string => {
    const quintile = Math.floor(value / 20);
    // Prevent 100-120
    if (quintile === 5) {
        return '80-100';
    }
    return `${quintile * 20}-${(quintile + 1) * 20}`;
}

export const METRICS: Record<string, MetricDef[]> = {
    'Mobility Poverty Index': [
        {
            id: 'IMPT_entropy_pca',
            label: 'IMPT with entropy', category: 'Mobility Poverty Index',
            description: 'Composite index using entropy, which gives more weight to variables that show the most contrast',
            format: (v) => (v || 0).toFixed(1),
            higherTheBetter: true, showDetails: true, showDetailsOnlyWhenSelected: true, default: true
        },
        {
            id: 'IMPT_score_pca_geom',
            label: 'IMPT with geometric mean', category: 'Mobility Poverty Index',
            description: 'Composite index using geometric mean, which penalizes low scores in any dimension, ensuring that a single low score significantly drags down the entire result',
            format: (v) => (v || 0).toFixed(1),
            higherTheBetter: true, showDetails: true, showDetailsOnlyWhenSelected: true
        },
        {
            id: 'IMPT_score_pca_avg',
            label: 'IMPT with equal weights', category: 'Mobility Poverty Index',
            description: 'Composite index considering equal weights for each dimension',
            format: (v) => (v || 0).toFixed(1),
            higherTheBetter: true, showDetails: true, showDetailsOnlyWhenSelected: true
        },
        {
            id: 'IMPT_dynamic',
            label: 'IMPT dynamic (custom weights)', category: 'Mobility Poverty Index',
            description: 'Computed on the fly based on user weightings of dimensions',
            format: (v) => (v || 0).toFixed(1),
            higherTheBetter: true, showDetails: true, showDetailsOnlyWhenSelected: true,
            isCalculated: true
        },
    ],
    'Dimensions': [
        {
            id: 'Accessibility_Index',
            label: 'Accessibility', category: 'Dimensions', icon: '🏘️',
            description: 'Aggregated accessibility metric, measuring access to key services and opportunities',
            format: (v) => getQuintileRange(v || 0),
            higherTheBetter: true, showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Mobility_Index',
            label: 'Mobility', category: 'Dimensions', icon: '🚲',
            description: 'Aggregated mobility metric, considering commuting and mobility infrastructure',
            format: (v) => getQuintileRange(v || 0),
            higherTheBetter: true, showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Safety_Index',
            label: 'Safety', category: 'Dimensions', icon: '🛡️',
            description: 'Aggregated safety metric, considering accidents',
            format: (v) => getQuintileRange(v || 0),
            higherTheBetter: true, showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Affordability_Index',
            label: 'Affordability', category: 'Dimensions', icon: '💰',
            description: 'Aggregated affordability metric, considering income and housing costs',
            format: (v) => getQuintileRange(v || 0),
            higherTheBetter: true, showDetails: true,
            isContributory: true, defaultWeight: 0.25
        }
    ]
};

export const FLAT_METRICS = Object.values(METRICS).flat();

export const COLORS = {
    Sequential: ['#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b'],
    Danger: ['#fff5f0', '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d'],
    Divergent: {
        negative: ['#006837', '#31a354', '#78c679', '#c2e699', '#f7fcb9'],
        positive: ['#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#f03b20', '#bd0026']
    }
};

export const REGIONS = {
    'metropolis': { name: "Metro Area", center: [38.74, -9.14], zoom: 11 },
    'PT1B': { name: "Grande Lisboa", center: [38.85, -9.15], zoom: 11 },
    'PT1A': { name: "Península de Setúbal", center: [38.55, -9.05], zoom: 11 }
} as const;

export const MODES = [
    { id: 'all', label: 'All', suffix: '', icon: '🌐' },
    { id: 'bike', label: 'Bike', suffix: '_bike', icon: '🚲' },
    { id: 'car', label: 'Car', suffix: '_car', icon: '🚗' },
    { id: 'pt', label: 'PT', suffix: '_pt', icon: '🚍' },
    { id: 'walk', label: 'Walk', suffix: '_walk', icon: '🚶' }
] as const;

export type ModeId = (typeof MODES)[number]['id'];
export type RegionKey = keyof typeof REGIONS;
export const REGION_KEYS = Object.keys(REGIONS) as RegionKey[];
export const DEFAULT_REGION: RegionKey = REGION_KEYS[0];
