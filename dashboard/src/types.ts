export type ViewLevel = 'hex' | 'freguesia' | 'municipality';

export type ScaleMethod = (values: number[], steps?: number) => number[];

export type MetricDef = {
    // Variable identifiers
    id: string;
    id_optional?: string; // Optional alternative ID (e.g. for different aggregation levels, such as mobility_cost_health_n1 for grid and mobility_cost_health_n1_residents for parish and municipality)
    id_variations?: {
        // ID variations selectable by user (e.g. for different time periods)
        // Groups are prensent at id or id_optional through {group} placeholder
        // e.g. mobility_cost_{poi_type}_{n_opportunities}_{population} with groups poi_type: [recreation, schools], n_opportunities: [n1,n2,n3], population: [kids, residents]
        // Some might be optional, in that case id definition is appended with :optional, example mobility_cost_{poi_type}_{n_opportunities}_{population:optional}
        [group: string]: string[];
    };
    valid_id_variations?: Record<string, string>[]; // List of explicit valid combinations. If defined, constrains selections.
    label: string;
    category: string;
    icon?: string;
    description?: string;
    pallete: string[];
    unit?: string;
    default?: boolean; // Show by default
    // Display
    scaleMethod?: ScaleMethod;
    scaleMinEqualsMax?: boolean; // If true, the scale will be [a,b], such that a = -b and |a| = |b| and the midpoint is 0
    steps?: number; // Number of scale breaks for discrete scales (e.g. 5 for quintiles, 10 for deciles)
    ignoreValues?: any[]; // Values to ignore when computing the scale
    format: (v: number, min: number, max: number) => string;
    // Area details sidebar (opens on the right, when user clicks on a feature)
    showAlwaysOnDetails?: boolean; // If true, the variable is always visible in the details sidebar. By default, variables are only visible when selected.
    // IMPT Calculation
    isCalculated?: boolean; // Identifies this metric as a dynamically computed index
    isContributory?: boolean; // Identifies that this metric can be used to compute a dynamic index
    defaultWeight?: number; // Default weight to start with (e.g. 0.25)
};
