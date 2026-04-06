import { MetricDef, ViewLevel } from './types';

export const LEVEL_CONFIG: Record<ViewLevel, { file: string, parent?: ViewLevel }> = {
    'municipality': { file: 'data/municipios_aggregated.geojson' },
    'freguesia': { file: 'data/freguesias_aggregated.geojson', parent: 'municipality' },
    'hex': { file: 'data/grid_aggregated.geojson', parent: 'freguesia' }
};

export const getQuintileRange = (value: number): string => {
    const quintile = Math.floor(value / 20);
    // Prevent 100-120
    if (quintile === 5) {
        return '80-100';
    }
    return `${quintile * 20}-${(quintile + 1) * 20}`;
}

export const COLORS = {
    // Low values red, high values green
    RedToGreen: ["#a50026", "#a70226", "#a90426", "#ab0626", "#ad0826", "#af0926", "#b10b26", "#b30d26", "#b50f26", "#b61127", "#b81327", "#ba1527", "#bc1727", "#be1927", "#c01b27", "#c21d28", "#c41f28", "#c52128", "#c72328", "#c92529", "#cb2729", "#cc2929", "#ce2b2a", "#d02d2a", "#d12f2b", "#d3312b", "#d4332c", "#d6352c", "#d7382d", "#d93a2e", "#da3c2e", "#dc3e2f", "#dd4030", "#de4331", "#e04532", "#e14733", "#e24a33", "#e34c34", "#e44e35", "#e55136", "#e75337", "#e85538", "#e95839", "#ea5a3a", "#eb5d3c", "#ec5f3d", "#ed613e", "#ed643f", "#ee6640", "#ef6941", "#f06b42", "#f16e43", "#f17044", "#f27346", "#f37547", "#f37848", "#f47a49", "#f57d4a", "#f57f4b", "#f6824d", "#f6844e", "#f7864f", "#f78950", "#f88b51", "#f88e53", "#f89054", "#f99355", "#f99556", "#f99858", "#fa9a59", "#fa9c5a", "#fa9f5b", "#fba15d", "#fba35e", "#fba660", "#fba861", "#fcaa62", "#fcad64", "#fcaf65", "#fcb167", "#fcb368", "#fcb56a", "#fdb86b", "#fdba6d", "#fdbc6e", "#fdbe70", "#fdc071", "#fdc273", "#fdc474", "#fdc676", "#fdc878", "#fdca79", "#fecc7b", "#fecd7d", "#fecf7e", "#fed180", "#fed382", "#fed584", "#fed685", "#fed887", "#feda89", "#fedb8b", "#fedd8d", "#fede8f", "#fee090", "#fee192", "#fee394", "#fee496", "#fee698", "#fee79a", "#fee89b", "#feea9d", "#feeb9f", "#feeca0", "#feeda2", "#feeea3", "#fdefa5", "#fdf0a6", "#fdf1a7", "#fdf2a9", "#fcf3aa", "#fcf4ab", "#fcf5ab", "#fbf5ac", "#fbf6ad", "#faf6ad", "#faf7ad", "#f9f7ae", "#f8f7ae", "#f7f8ad", "#f7f8ad", "#f6f8ad", "#f5f8ac", "#f4f8ab", "#f3f8ab", "#f1f8aa", "#f0f7a9", "#eff7a8", "#eef7a6", "#edf6a5", "#ebf6a4", "#eaf6a2", "#e8f5a1", "#e7f59f", "#e6f49d", "#e4f39c", "#e2f39a", "#e1f298", "#dff297", "#def195", "#dcf093", "#daef92", "#d9ef90", "#d7ee8e", "#d5ed8d", "#d3ec8b", "#d2ec89", "#d0eb88", "#ceea86", "#cce985", "#cae983", "#c8e882", "#c6e780", "#c4e67f", "#c2e57e", "#c0e47c", "#bee47b", "#bce37a", "#bae279", "#b8e178", "#b6e076", "#b4df75", "#b2de74", "#b0dd73", "#aedc72", "#acdb71", "#a9da70", "#a7d970", "#a5d86f", "#a3d86e", "#a0d76d", "#9ed66c", "#9cd56c", "#99d36b", "#97d26b", "#95d16a", "#92d069", "#90cf69", "#8ece68", "#8bcd68", "#89cc67", "#86cb67", "#84ca66", "#81c966", "#7fc866", "#7cc665", "#79c565", "#77c464", "#74c364", "#71c263", "#6fc063", "#6cbf62", "#69be62", "#67bd62", "#64bc61", "#61ba60", "#5eb960", "#5cb85f", "#59b65f", "#56b55e", "#53b45e", "#51b25d", "#4eb15c", "#4baf5c", "#48ae5b", "#46ad5a", "#43ab5a", "#40aa59", "#3da858", "#3ba757", "#38a557", "#36a456", "#33a255", "#31a154", "#2e9f54", "#2c9d53", "#2a9c52", "#289a51", "#259950", "#23974f", "#21954f", "#1f944e", "#1e924d", "#1c904c", "#1a8f4b", "#188d4a", "#178b49", "#158948", "#148747", "#128646", "#118446", "#108245", "#0e8044", "#0d7e43", "#0c7d42", "#0b7b41", "#0a7940", "#08773f", "#07753e", "#06733d", "#05713c", "#04703b", "#036e3a", "#026c39", "#016a38", "#006837"],
    // Low values green, high values red
    GreenToRed: ["#a50026", "#a70226", "#a90426", "#ab0626", "#ad0826", "#af0926", "#b10b26", "#b30d26", "#b50f26", "#b61127", "#b81327", "#ba1527", "#bc1727", "#be1927", "#c01b27", "#c21d28", "#c41f28", "#c52128", "#c72328", "#c92529", "#cb2729", "#cc2929", "#ce2b2a", "#d02d2a", "#d12f2b", "#d3312b", "#d4332c", "#d6352c", "#d7382d", "#d93a2e", "#da3c2e", "#dc3e2f", "#dd4030", "#de4331", "#e04532", "#e14733", "#e24a33", "#e34c34", "#e44e35", "#e55136", "#e75337", "#e85538", "#e95839", "#ea5a3a", "#eb5d3c", "#ec5f3d", "#ed613e", "#ed643f", "#ee6640", "#ef6941", "#f06b42", "#f16e43", "#f17044", "#f27346", "#f37547", "#f37848", "#f47a49", "#f57d4a", "#f57f4b", "#f6824d", "#f6844e", "#f7864f", "#f78950", "#f88b51", "#f88e53", "#f89054", "#f99355", "#f99556", "#f99858", "#fa9a59", "#fa9c5a", "#fa9f5b", "#fba15d", "#fba35e", "#fba660", "#fba861", "#fcaa62", "#fcad64", "#fcaf65", "#fcb167", "#fcb368", "#fcb56a", "#fdb86b", "#fdba6d", "#fdbc6e", "#fdbe70", "#fdc071", "#fdc273", "#fdc474", "#fdc676", "#fdc878", "#fdca79", "#fecc7b", "#fecd7d", "#fecf7e", "#fed180", "#fed382", "#fed584", "#fed685", "#fed887", "#feda89", "#fedb8b", "#fedd8d", "#fede8f", "#fee090", "#fee192", "#fee394", "#fee496", "#fee698", "#fee79a", "#fee89b", "#feea9d", "#feeb9f", "#feeca0", "#feeda2", "#feeea3", "#fdefa5", "#fdf0a6", "#fdf1a7", "#fdf2a9", "#fcf3aa", "#fcf4ab", "#fcf5ab", "#fbf5ac", "#fbf6ad", "#faf6ad", "#faf7ad", "#f9f7ae", "#f8f7ae", "#f7f8ad", "#f7f8ad", "#f6f8ad", "#f5f8ac", "#f4f8ab", "#f3f8ab", "#f1f8aa", "#f0f7a9", "#eff7a8", "#eef7a6", "#edf6a5", "#ebf6a4", "#eaf6a2", "#e8f5a1", "#e7f59f", "#e6f49d", "#e4f39c", "#e2f39a", "#e1f298", "#dff297", "#def195", "#dcf093", "#daef92", "#d9ef90", "#d7ee8e", "#d5ed8d", "#d3ec8b", "#d2ec89", "#d0eb88", "#ceea86", "#cce985", "#cae983", "#c8e882", "#c6e780", "#c4e67f", "#c2e57e", "#c0e47c", "#bee47b", "#bce37a", "#bae279", "#b8e178", "#b6e076", "#b4df75", "#b2de74", "#b0dd73", "#aedc72", "#acdb71", "#a9da70", "#a7d970", "#a5d86f", "#a3d86e", "#a0d76d", "#9ed66c", "#9cd56c", "#99d36b", "#97d26b", "#95d16a", "#92d069", "#90cf69", "#8ece68", "#8bcd68", "#89cc67", "#86cb67", "#84ca66", "#81c966", "#7fc866", "#7cc665", "#79c565", "#77c464", "#74c364", "#71c263", "#6fc063", "#6cbf62", "#69be62", "#67bd62", "#64bc61", "#61ba60", "#5eb960", "#5cb85f", "#59b65f", "#56b55e", "#53b45e", "#51b25d", "#4eb15c", "#4baf5c", "#48ae5b", "#46ad5a", "#43ab5a", "#40aa59", "#3da858", "#3ba757", "#38a557", "#36a456", "#33a255", "#31a154", "#2e9f54", "#2c9d53", "#2a9c52", "#289a51", "#259950", "#23974f", "#21954f", "#1f944e", "#1e924d", "#1c904c", "#1a8f4b", "#188d4a", "#178b49", "#158948", "#148747", "#128646", "#118446", "#108245", "#0e8044", "#0d7e43", "#0c7d42", "#0b7b41", "#0a7940", "#08773f", "#07753e", "#06733d", "#05713c", "#04703b", "#036e3a", "#026c39", "#016a38", "#006837"].reverse(),
    Divergent: {
        negative: ['#006837', '#31a354', '#78c679', '#c2e699', '#f7fcb9'],
        positive: ['#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#f03b20', '#bd0026']
    }
};

const METRIC_DATA: Record<string, Omit<MetricDef, 'category'>[]> = {
    'metrics.categories.mobility_poverty_index': [
        {
            id: 'IMPT_entropy_pca',
            label: 'metrics.impt_entropy.label',
            description: 'metrics.impt_entropy.description',
            pallete: COLORS.GreenToRed,
            format: (v) => (v || 0).toFixed(1),
            showDetails: true, showDetailsOnlyWhenSelected: true
        },
        {
            id: 'IMPT_score_pca_geom',
            label: 'metrics.impt_geom.label',
            description: 'metrics.impt_geom.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed,
            showDetails: true, showDetailsOnlyWhenSelected: true, default: true
        },
        {
            id: 'IMPT_score_pca_avg',
            label: 'metrics.impt_equal.label',
            description: 'metrics.impt_equal.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed,
            showDetails: true, showDetailsOnlyWhenSelected: true
        },
        {
            id: 'IMPT_dynamic',
            label: 'metrics.impt_dynamic.label',
            description: 'metrics.impt_dynamic.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed,
            showDetails: true, showDetailsOnlyWhenSelected: true,
            isCalculated: true
        },
    ],
    'metrics.categories.dimensions': [
        {
            id: 'Accessibility_Index',
            label: 'metrics.accessibility.label', icon: '🏘️',
            description: 'metrics.accessibility.description',
            format: (v) => getQuintileRange(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Mobility_Index',
            label: 'metrics.mobility.label', icon: '🚲',
            description: 'metrics.mobility.description',
            format: (v) => getQuintileRange(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Safety_Index',
            label: 'metrics.safety.label', icon: '🛡️',
            description: 'metrics.safety.description',
            format: (v) => getQuintileRange(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Affordability_Index',
            label: 'metrics.affordability.label', icon: '💰',
            description: 'metrics.affordability.description',
            format: (v) => getQuintileRange(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        }
    ],
    'metrics.categories.safety': [
        // safety_total_acidentes, safety_indice_gravidade, safety_inner_total_acidentes, safety_inner_indice_gravidade
        {
            id: 'safety_total_acidentes',
            label: 'metrics.safety_total_acidentes',
            description: 'metrics.safety_total_acidentes.description',
            format: (v) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'safety_indice_gravidade',
            label: 'metrics.safety_indice_gravidade',
            description: 'metrics.safety_indice_gravidade.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'safety_inner_total_acidentes',
            label: 'metrics.safety_inner_total_acidentes',
            description: 'metrics.safety_inner_total_acidentes.description',
            format: (v) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'safety_inner_indice_gravidade',
            label: 'metrics.safety_inner_indice_gravidade',
            description: 'metrics.safety_inner_indice_gravidade.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        }
    ],
    'metrics.categories.affordability': [
        {
            id: 'affordability_total_money',
            label: 'metrics.affordability_total_money.label',
            description: 'metrics.affordability_total_money.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed
        }
    ],
    // census
    'metrics.categories.census_population': [
        {
            id: 'modal_census_share',
            label: 'metrics.modal_census_share',
            description: 'metrics.modal_census_share.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_income_income_hh',
            label: 'metrics.census_income_income_hh',
            description: 'metrics.census_income_income_hh.description',
            format: (v) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_income_gini_coef',
            label: 'metrics.census_income_gini_coef',
            description: 'metrics.census_income_gini_coef.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_income_housing_costs',
            label: 'metrics.census_income_housing_costs',
            description: 'metrics.census_income_housing_costs.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_population',
            label: 'metrics.census_landuse_population',
            description: 'metrics.census_landuse_population.description',
            format: (v) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_population_density',
            label: 'metrics.census_landuse_population_density',
            description: 'metrics.census_landuse_population_density.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_youth_ratio',
            label: 'metrics.census_landuse_youth_ratio',
            description: 'metrics.census_landuse_youth_ratio.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_elderly_ratio',
            label: 'metrics.census_landuse_elderly_ratio',
            description: 'metrics.census_landuse_elderly_ratio.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_women_percentage',
            label: 'metrics.census_landuse_women_percentage',
            description: 'metrics.census_landuse_women_percentage.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        }
    ],
    'metrics.categories.census_landuse': [
        {
            id: 'census_landuse_buildings',
            label: 'metrics.census_landuse_buildings',
            description: 'metrics.census_landuse_buildings.description',
            format: (v) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_buildings_pre1945_percentage',
            label: 'metrics.census_landuse_buildings_pre1945_percentage',
            description: 'metrics.census_landuse_buildings_pre1945_percentage.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_buildings_volume_m3',
            label: 'metrics.census_landuse_buildings_volume_m3',
            description: 'metrics.census_landuse_buildings_volume_m3.description',
            format: (v) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_landuse_volume_density',
            label: 'metrics.census_landuse_volume_density',
            description: 'metrics.census_landuse_volume_density.description',
            format: (v) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        }
    ]
};

export const METRICS = Object.freeze(
    Object.fromEntries(
        Object.entries(METRIC_DATA).map(([category, metrics]) => [
            category,
            metrics.map(m => ({ ...m, category } as MetricDef))
        ])
    )
) as Record<string, MetricDef[]>;


export const FLAT_METRICS = Object.values(METRICS).flat();

export const REGIONS = {
    'metropolis': { name: "regions.metropolis", center: [38.74, -9.10], zoom: 11 },
    'PT1B': { name: "regions.grand_lisbon", center: [38.85, -9.15], zoom: 11 },
    'PT1A': { name: "regions.setubal_peninsula", center: [38.65, -8.90], zoom: 11 }
} as const;

export const MODES = [
    // suffix is added to METRICS.id to get the metric for that mode
    // > Based on it we can determine if the metric exists for that mode and display the mode selector accordingly
    // suffixFallback is used only on IMPT dynamic computation 
    // > It tells that for that mode, if no metric with suffix, suffixFallback metric should be used instead 
    { id: 'all', label: 'modes.all', suffix: '', icon: '🌐' },
    { id: 'all_pass', label: 'modes.all_pass', suffix: '_pass', suffixFallback: '', icon: '🌐' },
    { id: 'all_no_pass', label: 'modes.all_no_pass', suffix: '_no_pass', suffixFallback: '', icon: '🌐' },
    { id: 'pt', label: 'modes.pt', suffix: '_pt', icon: '🚍' },
    { id: 'pt_pass', label: 'modes.pt_pass', suffix: '_pt_pass', suffixFallback: '_pt', icon: '🚍' },
    { id: 'pt_no_pass', label: 'modes.pt_no_pass', suffix: '_pt_no_pass', suffixFallback: '_pt', icon: '🚍' },
    { id: 'walk', label: 'modes.walk', suffix: '_walk', icon: '🚶' },
    { id: 'bike', label: 'modes.bike', suffix: '_bike', icon: '🚲' },
    { id: 'car', label: 'modes.car', suffix: '_car', icon: '🚗' }
] as const;

export type ModeId = (typeof MODES)[number]['id'];
export type RegionKey = keyof typeof REGIONS;
export const REGION_KEYS = Object.keys(REGIONS) as RegionKey[];
export const DEFAULT_REGION: RegionKey = REGION_KEYS[0];

export const MAP_LAYERS = [
    {
        id: 'carto',
        label: 'map.layer_carto',
        icon: '',
        attribution: '&copy; CARTO',
        getUrl: (isDark: boolean) => isDark
            ? "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png"
            : "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
    },
    {
        id: 'satellite',
        label: 'map.layer_satellite',
        icon: '',
        attribution: '&copy; ESRI',
        url: "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    },
    {
        id: 'osm',
        label: 'map.layer_osm',
        icon: '',
        attribution: '&copy; OpenStreetMap',
        url: "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    }
];
