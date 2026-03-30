export type ViewLevel = 'hex' | 'freguesia' | 'municipality';

export type MetricDef = {
    id: string;
    label: string;
    category: string;
    icon?: string;
    description?: string;
    format: (v: number) => string;
    pallete: string[];
    unit?: string;
    viewLevel?: 'municipality' | 'all'; // Restrict visibility
    isFake?: boolean; // Label as placeholder data
    default?: boolean; // Show by default
    // Area details sidebar (opens on the right, when user clicks on a feature)
    showDetails?: boolean; // Show in "Area details" section
    showDetailsOnlyWhenSelected?: boolean; // Show in details only when active
    // IMPT Calculation
    isCalculated?: boolean; // Identifies this metric as a dynamically computed index
    isContributory?: boolean; // Identifies that this metric can be used to compute a dynamic index
    defaultWeight?: number; // Default weight to start with (e.g. 0.25)
};
