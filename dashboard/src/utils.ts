import { continuousScale, equalIntervalScale } from "./constants";
import { MetricDef } from "./types";


/**
 * Checks if a value should be ignored based on metric settings and common data issues (null, undefined, NaN).
 */

export const isMetricValueIgnored = (val: any, metric: MetricDef): boolean => {
    return val === null || val === undefined || isNaN(Number(val)) || (metric.ignoreValues?.includes(val) ?? false);
};
/**
 * Generates a scale (domain or breaks) based on the metric's scaling method.
 */

export const getMetricDomain = (values: number[], metric: MetricDef): number[] => {
    // Filter out ignored values, nulls, and NaNs
    const sorted = [...values]
        .filter(v => !isMetricValueIgnored(v, metric))
        .sort((a, b) => a - b);

    if (sorted.length === 0) return [0, 1];

    const method = metric.scaleMethod || continuousScale;

    let scale = method(sorted, metric.steps);

    if (metric.scaleMinEqualsMax) {
        const maxAbs = Math.max(Math.abs(scale[0]), Math.abs(scale[scale.length - 1]));
        if (method === equalIntervalScale) {
            scale = equalIntervalScale([-maxAbs, maxAbs], metric.steps);
        } else if (method === continuousScale || scale.length === 2) {
            scale = [-maxAbs, maxAbs];
        } else {
            scale[0] = -maxAbs;
            scale[scale.length - 1] = maxAbs;
        }
    }

    console.log("Scale for ", metric.id, scale, values);
    
    if (metric.scaleMin !== undefined) scale[0] = metric.scaleMin;
    if (metric.scaleMax !== undefined) scale[scale.length - 1] = metric.scaleMax;

    return scale;
};
/**
 * Maps a value to a palette index based on the domain type (discrete vs continuous).
 */
const getPaletteIndex = (val: number, domain: number[], paletteLength: number): number => {
    if (domain.length > 2) {
        // Discrete scale breaks (quantiles/slices)
        let sliceIdx = 0;
        for (let i = 0; i < domain.length - 1; i++) {
            if (val >= domain[i] && val <= domain[i + 1]) {
                sliceIdx = i;
                break;
            }
        }
        const nSlices = domain.length - 1;
        return Math.min(Math.floor((sliceIdx / nSlices) * paletteLength), paletteLength - 1);
    } else {
        // Continuous linear interpolation mapping
        const [min, max] = domain;
        const range = max - min;
        if (range === 0) return Math.floor(paletteLength / 2);
        const norm = Math.max(0, Math.min(1, (val - min) / range));
        return Math.min(Math.floor(norm * paletteLength), paletteLength - 1);
    }
};
/**
 * Returns the color for a given value based on the metric palette and domain.
 */

export const getColor = (val: number, domain: number[], metric: MetricDef): string => {
    if (isMetricValueIgnored(val, metric)) return 'rgba(0,0,0,0.05)';
    const palette = metric.pallete;
    const idx = getPaletteIndex(val, domain, palette.length);
    return palette[idx];
};
/**
 * Generates a CSS linear-gradient for the legend representation of a metric.
 */

export const getLegendGradient = (metric: MetricDef, domain: number[]): string => {
    const palette = metric.pallete;
    if (domain.length > 2) {
        const nSlices = domain.length - 1;
        const gradientParts = [];
        for (let i = 0; i < nSlices; i++) {
            const paletteIdx = Math.min(Math.floor((i / nSlices) * palette.length), palette.length - 1);
            const color = palette[paletteIdx];
            const start = (i / nSlices) * 100;
            const end = ((i + 1) / nSlices) * 100;
            gradientParts.push(`${color} ${start}% ${end}%`);
        }
        return `linear-gradient(to right, ${gradientParts.join(', ')})`;
    }
    // Continuous colormaps: pick 5 equidistant stops for CSS representation
    const stops = [0, 0.25, 0.5, 0.75, 1];
    const colors = stops.map(s => {
        const idx = Math.min(Math.floor(s * palette.length), palette.length - 1);
        return palette[idx];
    });
    return `linear-gradient(to right, ${colors.join(', ')})`;
};

/**
 * Resolves the value of a metric from a feature's properties, considering 
 * mode suffixes, fallbacks, and optional alternative IDs.
 */
export const getMetricValue = (properties: any, metric: MetricDef, mode: { suffix?: string, suffixFallback?: string }, variations?: Record<string, string>): any => {
    if (!properties) return undefined;

    const resolveId = (id: string) => {
        let resolved = id;
        if (variations) {
            Object.entries(variations).forEach(([group, value]) => {
                resolved = resolved.replace(`{${group}}`, value);
            });
        }
        return resolved;
    }

    const idsToTry = [
        resolveId(metric.id) + (mode.suffix || ''),
        mode.suffixFallback !== undefined ? resolveId(metric.id) + mode.suffixFallback : undefined,
        resolveId(metric.id),
        metric.id_optional ? resolveId(metric.id_optional) + (mode.suffix || '') : undefined,
        (metric.id_optional && mode.suffixFallback !== undefined) ? resolveId(metric.id_optional) + mode.suffixFallback : undefined,
        metric.id_optional ? resolveId(metric.id_optional) : undefined
    ].filter(Boolean) as string[];

    for (const id of idsToTry) {
        if (properties[id] !== undefined) {
            return properties[id];
        }
    }
    return undefined;
};

/**
 * Dynamically discovers valid variation combinations for a metric by scanning feature properties.
 */
export const discoverMetricVariations = (metric: MetricDef, features: any[]): Record<string, string>[] => {
    if (!metric.id_variations || !features || features.length === 0) return [];

    const groups = Object.keys(metric.id_variations);
    const validCombinations: Record<string, string>[] = [];
    const seenCombos = new Set<string>();
    const seenKeys = new Set<string>();

    const patterns = [metric.id, metric.id_optional].filter(Boolean) as string[];

    // Build regexes for each pattern
    const regexes = patterns.map(pattern => {
        let regexSource = pattern.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        groups.forEach(group => {
            // Constrain match to ONLY the predefined options to avoid capturing mode suffixes
            const options = metric.id_variations![group]
                .slice()
                .sort((a: string, b: string) => b.length - a.length)
                .map((o: string) => o.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'))
                .join('|');
            regexSource = regexSource.replace(`\\{${group}\\}`, `(?<${group}>${options})`);
        });
        return new RegExp(`^${regexSource}(?:_.*)?$`);
    });

    // Sample features to find valid keys (scanning all for robustness, but only unique keys)
    features.forEach(f => {
        if (!f.properties) return;
        Object.keys(f.properties).forEach(key => {
            if (seenKeys.has(key)) return;
            seenKeys.add(key);

            for (const regex of regexes) {
                const match = key.match(regex);
                if (match && match.groups) {
                    const combo: Record<string, string> = {};
                    let allMatched = true;
                    groups.forEach(g => {
                        if (match.groups![g]) {
                            combo[g] = match.groups![g];
                        } else {
                            allMatched = false;
                        }
                    });

                    if (allMatched) {
                        const comboKey = Object.entries(combo).sort().map(e => e.join(':')).join('|');
                        if (!seenCombos.has(comboKey)) {
                            seenCombos.add(comboKey);
                            validCombinations.push(combo);
                        }
                    }
                }
            }
        });
    });

    console.log("Valid combinations for ", metric.id, validCombinations);

    return validCombinations;
};

