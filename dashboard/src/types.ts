import React from 'react';
import { ModeId } from './constants';

export type ViewLevel = 'hex' | 'freguesia' | 'municipality';

export type VariationGroup = string[] | {
    options: string[];
    modes?: ModeId[]; // If defined, the options are only available for the specified modes
    viewLevels?: ViewLevel[]; // If defined, the options are only available for the specified view levels
    formSlider?: boolean; // If true, the options are presented as a slider
    visible?: boolean; // If false, the options are not visible
};

export type ScaleMethod = (values: number[], steps?: number) => number[];

export type MetricDef = {
    // Variable identifiers
    id: string;
    id_variations?: {
        // ID variations selectable by user (e.g. for different time periods)
        // Groups are prensent at id through {group} placeholder
        // e.g. mobility_cost_{poi_type}_{n_opportunities}_{population} with groups poi_type: [recreation, schools], n_opportunities: [n1,n2,n3], population: [kids, residents]
        [group: string]: VariationGroup;
    };
    valid_id_variations?: Record<string, string>[]; // List of explicit valid combinations. If defined, constrains selections.
    label: string;
    category: string;
    icon?: string;
    description?: string;
    pallete: string[];
    sources?: string[];
    unit?: (mode: ModeId, value?: number) => string;
    default?: boolean; // Show by default
    // Display
    scaleMethod?: ScaleMethod;
    scaleMinEqualsMax?: boolean; // If true, the scale will be [a,b], such that a = -b and |a| = |b| and the midpoint is 0
    steps?: number; // Number of scale breaks for discrete scales (e.g. 5 for quintiles, 10 for deciles)
    ignoreValues?: any[]; // Values to ignore when computing the scale
    scaleMin?: number; // Force a minimum value for the scale domain
    scaleMax?: number; // Force a maximum value for the scale domain
    format: (v: number, min: number, max: number, mode: ModeId) => string;
    legendCategories?: { color: string; label: string }[]; // Specific legend categories for non-continuous layers
    // Area details sidebar (opens on the right, when user clicks on a feature)
    showAlwaysOnDetails?: boolean; // If true, the variable is always visible in the details sidebar. By default, variables are only visible when selected.
    // IMPT Calculation
    isCalculated?: boolean; // Identifies this metric as a dynamically computed index
    isContributory?: boolean; // Identifies that this metric can be used to compute a dynamic index
    defaultWeight?: number; // Default weight to start with (e.g. 0.25)
    // External data
    auxiliaryDataUrl?: string; // URL to fetch additional data when selected
    renderAuxiliaryData?: (data: any, metricId: string, t: (key: string) => string, limit?: number) => React.ReactNode;
};
