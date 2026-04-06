import { useState, useMemo, useEffect } from 'react';
import { MapContainer, TileLayer, GeoJSON, Pane } from 'react-leaflet';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import { Loader2, AlertTriangle, Activity, Layers, Globe, Zap, RocketIcon } from 'lucide-react';
import { useTranslation } from 'react-i18next';

import { ViewLevel, MetricDef } from './types';
import { METRICS, FLAT_METRICS, COLORS, REGION_KEYS, REGIONS, DEFAULT_REGION, MODES, RegionKey, ModeId, LEVEL_CONFIG, MAP_LAYERS } from './constants';
import { ZoomHandler, SelectedFeatureCentering, MapDeselectHandler } from './components/MapHandlers';
import { AHPModal } from './components/AHPModal';
import { SidebarLeft } from './components/SidebarLeft';
import { SidebarRight } from './components/SidebarRight';
import { AboutModal } from './components/AboutModal';
import { DownloadModal } from './components/DownloadModal';
import { MapFilterDropdown } from './components/MapFilterDropdown';
import { MobileOverlay } from './components/MobileOverlay';
import { MapTools } from './components/MapTools';

const Dashboard = () => {
    const { t, i18n } = useTranslation();
    const [viewLevel, setViewLevel] = useState<ViewLevel>('freguesia');
    const [nutFilter, setNutFilter] = useState<RegionKey>(DEFAULT_REGION);
    const [selectedMetricId, setSelectedMetricId] = useState<string>(FLAT_METRICS.find(m => m.default)?.id || FLAT_METRICS[0].id);
    const [selectedModeId, setSelectedModeId] = useState<ModeId>('all');
    const [selectedFeature, setSelectedFeature] = useState<any>(null);
    const [mapStyle, setMapStyle] = useState<string>('carto');
    const [zoomRequest, setZoomRequest] = useState<{ id: string | number, timestamp: number } | null>(null);
    const contributoryMetrics = useMemo(() => FLAT_METRICS.filter(m => m.isContributory), []);
    const defaultWeights: Record<string, number> = useMemo(() => {
        const initial: Record<string, number> = {};
        contributoryMetrics.forEach(m => {
            initial[m.id] = m.defaultWeight || 1;
        });
        return initial;
    }, [contributoryMetrics]);
    const [weights, setWeights] = useState<Record<string, number>>(defaultWeights);
    const [showAbout, setShowAbout] = useState(false);
    const [showDownload, setShowDownload] = useState(false);
    const [isDarkMode, setIsDarkMode] = useState(false);
    const [isAHPModalOpen, setIsAHPModalOpen] = useState(false);
    const [collapsedSections, setCollapsedSections] = useState<Record<string, boolean>>(() => {
        const keys = Object.keys(METRICS);
        return keys.slice(1).reduce((acc, key) => ({ ...acc, [key]: true }), {});
    });
    const [isMobile, setIsMobile] = useState(window.innerWidth < 1024);

    useEffect(() => {
        const handleResize = () => setIsMobile(window.innerWidth < 1024);
        window.addEventListener('resize', handleResize);
        return () => window.removeEventListener('resize', handleResize);
    }, []);

    const [dataState, setDataState] = useState<{
        geo: Record<string, any>; limits: any; loading: boolean; error: string | null;
        parentLookup: Record<string, string>;
    }>({ geo: {}, limits: null, loading: true, error: null, parentLookup: {} });

    useEffect(() => {
        const load = async () => {
            try {
                const levels = Object.keys(LEVEL_CONFIG) as ViewLevel[];
                const results = await Promise.all([
                    ...levels.map(l => fetch(LEVEL_CONFIG[l].file).then(r => r.json())),
                    fetch('data/municipios_limits.json').then(r => r.json())
                ]);

                const geo: Record<string, any> = {};
                levels.forEach((l, i) => { geo[l] = results[i]; });

                // Create parent lookups for each level that serves as a parent
                const parentLookup: Record<string, string> = {};
                levels.forEach(l => {
                    const parentLevel = LEVEL_CONFIG[l].parent;
                    if (parentLevel && geo[parentLevel]) {
                        geo[parentLevel].features.forEach((f: any) => {
                            // Store in a way that we can identify which parent level it belongs to if needed
                            // For now, a flat map of ID -> Name works if IDs are unique across parents
                            parentLookup[`${parentLevel}-${f.properties.id}`] = f.properties.name || f.properties.id;
                        });
                    }
                });

                setDataState({ geo, limits: results[levels.length], loading: false, error: null, parentLookup });
            } catch (err) {
                console.error(err);
                setDataState(s => ({ ...s, loading: false, error: "System encountered a data loading error." }));
            }
        };
        load();
    }, []);

    const selectedMetric = useMemo(() => FLAT_METRICS.find(m => m.id === selectedMetricId) || FLAT_METRICS[0], [selectedMetricId]);
    const selectedMode = useMemo(() => MODES.find(m => m.id === selectedModeId) || MODES[0], [selectedModeId]);

    // Helper to check if a specific mode is available for a metric at a certain view level
    const isModeAvailable = (modeId: string, metricId: string, level: string) => {
        const metric = FLAT_METRICS.find(m => m.id === metricId);
        let checkMetricId = metricId;

        // For calculated metrics, evaluate availability based on the default reference metric
        if (metric?.isCalculated) {
            const defaultMetric = FLAT_METRICS.find(m => m.default);
            if (defaultMetric) checkMetricId = defaultMetric.id;
        }

        const mode = MODES.find(m => m.id === modeId);
        if (!mode || !dataState.geo[level as ViewLevel]) return false;
        const feature = dataState.geo[level as ViewLevel]?.features[0];
        if (!feature) return false;
        const effectiveId = `${checkMetricId}${mode.suffix}`;
        return !!(feature.properties && feature.properties[effectiveId] !== undefined);
    };

    // Helper to check if a metric is available at a certain view level with the current mode
    const isMetricAvailable = (metricId: string, level: string, modeSuffix: string = selectedMode.suffix) => {
        const metric = FLAT_METRICS.find(m => m.id === metricId);
        let checkMetricId = metricId;

        // For calculated metrics, evaluate availability based on the default reference metric
        if (metric?.isCalculated) {
            const defaultMetric = FLAT_METRICS.find(m => m.default);
            if (defaultMetric) checkMetricId = defaultMetric.id;
        }

        if (!dataState.geo[level as ViewLevel]) return false;
        const feature = dataState.geo[level as ViewLevel]?.features[0];
        if (!feature) return false;
        const effectiveId = `${checkMetricId}${modeSuffix}`;
        return feature.properties && (feature.properties[effectiveId] !== undefined || feature.properties[checkMetricId] !== undefined);
    };

    // Derive effective level and mode to ensure consistent rendering even before useEffect synchronizes state
    const effectiveLevel = useMemo(() => {
        if (isMetricAvailable(selectedMetricId, viewLevel)) return viewLevel;
        return (['hex', 'freguesia', 'municipality'] as const).find(l => isMetricAvailable(selectedMetricId, l)) || viewLevel;
    }, [selectedMetricId, viewLevel, dataState.geo]);

    const effectiveMode = useMemo(() => {
        if (isModeAvailable(selectedModeId, selectedMetricId, effectiveLevel)) return selectedMode;
        // Fallback to first available mode for this metric
        const firstAvailable = MODES.find(m => isModeAvailable(m.id, selectedMetricId, effectiveLevel));
        return firstAvailable || MODES[0];
    }, [selectedModeId, selectedMetricId, effectiveLevel, dataState.geo, selectedMode]);

    // Auto-switch view level OR reset mode if not available for selected metric
    useEffect(() => {
        if (!isModeAvailable(selectedModeId, selectedMetricId, viewLevel)) {
            const firstAvailable = MODES.find(m => isModeAvailable(m.id, selectedMetricId, viewLevel));
            if (firstAvailable) {
                setSelectedModeId(firstAvailable.id as ModeId);
            }
        }

        if (!isMetricAvailable(selectedMetricId, viewLevel)) {
            const availableLevel = (['hex', 'freguesia', 'municipality'] as const).find(l => isMetricAvailable(selectedMetricId, l));
            if (availableLevel) {
                setViewLevel(availableLevel);
            }
        }
    }, [selectedMetricId, selectedModeId, viewLevel, dataState.geo]);

    const computedGeoData = useMemo(() => {
        let raw = dataState.geo[effectiveLevel];
        if (!raw || !raw.features) return { type: "FeatureCollection", features: [] };

        let features = raw.features;
        if (nutFilter !== REGION_KEYS[0]) {
            features = features.filter((f: any) => f.properties?.region_id === nutFilter || f.properties?.nuts2 === REGIONS[nutFilter].name);
        }

        // Inject dynamic IMPT metric computed on-the-fly based on user weights
        const dynamicMetricConfig = FLAT_METRICS.find(m => m.isCalculated);
        if (dynamicMetricConfig) {
            // Edge cases (should be validated)
            // (Walk || Bike) => No affordability considered
            // All && (Pass || NoPass) => Fallback on accessibility, mobility and safety
            // PT && (Pass || NoPass) => Fallback on accessibility, mobility AND no safety

            const dynamicId = `${dynamicMetricConfig.id}${effectiveMode.suffix}`;
            const fallbackId = effectiveMode.suffixFallback ? `${dynamicMetricConfig.id}${effectiveMode.suffixFallback}` : undefined;
            console.log("Computing dynamic IMPT for", dynamicId, "(" + fallbackId + ")", dynamicMetricConfig, effectiveMode, weights);

            features = features.map((f: any, i: number) => {
                let computedIndex = 0;

                Object.entries(weights).forEach(([metricId, weight]) => {
                    const effectiveMetricId = `${metricId}${effectiveMode.suffix}`;
                    const fallbackMetricId = effectiveMode.suffixFallback !== undefined ? `${metricId}${effectiveMode.suffixFallback}` : undefined;
                    // Use a fallback to the base metric ID if the mode-specific suffix version is missing
                    const val = f.properties[effectiveMetricId] ?? f.properties[fallbackMetricId] ?? undefined;
                    if (val !== undefined) {
                        computedIndex += val * weight;
                    }
                    if (val !== undefined && i == 0) {
                        if (f.properties[effectiveMetricId]) {
                            console.log("> Considering metric", effectiveMetricId, "with weight", weight);
                        } else if (f.properties[fallbackMetricId]) {
                            console.warn("> Considering metric", fallbackMetricId, "with weight", weight, "(using fallback)");
                        } else {
                            console.error("Debug error")
                        }
                    } else if (i == 0) {
                        console.log("> No value for metric", metricId, val);
                    }
                });

                // Invert index, a lowed ranking in aggregated dimensions, means more poverty, and therefore a higher IMPT
                computedIndex = 100 - computedIndex;

                return {
                    ...f,
                    properties: {
                        ...f.properties,
                        [dynamicId]: computedIndex
                    }
                };
            });
        }

        return { ...raw, features };
    }, [effectiveLevel, nutFilter, dataState, weights, effectiveMode.suffix]);
    const filteredLimits = useMemo(() => {
        if (!dataState.limits) return null;
        if (nutFilter === REGION_KEYS[0]) return dataState.limits;

        return {
            ...dataState.limits,
            features: dataState.limits.features.filter((f: any) => f.properties?.region_id === nutFilter)
        };
    }, [dataState.limits, nutFilter]);

    const currentDomain = useMemo(() => {
        const defaultDomain = [0, 1];
        if (!computedGeoData?.features?.length) return defaultDomain;
        const effectiveId = `${selectedMetric.id}${effectiveMode.suffix}`;
        const values = computedGeoData.features
            .map((f: any) => f.properties?.[effectiveId])
            .filter((v: any) => v !== undefined && !isNaN(v) && !(selectedMetric.ignoreValues?.includes(v)));
        if (values.length === 0) return defaultDomain;

        const min = Math.min(...values);
        const max = Math.max(...values);

        // If slices are defined, compute quantile-based breaks
        if (selectedMetric.quantiles && selectedMetric.quantiles > 1) {
            const sorted = [...values].sort((a, b) => a - b);
            const n = selectedMetric.quantiles;
            const quants = [];
            for (let i = 0; i <= n; i++) {
                const idx = Math.min(Math.floor((i / n) * (sorted.length - 1)), sorted.length - 1);
                quants.push(sorted[idx]);
            }
            console.log("Quants for " + selectedMetric.id, quants);
            return quants;
        }

        return [min, max];
    }, [computedGeoData, selectedMetric, effectiveMode]);

    // Precompute domains for all metrics shown in details to support dynamic coloring
    const allDomains = useMemo(() => {
        const result: Record<string, number[]> = {};
        if (!computedGeoData?.features?.length) return result;
        FLAT_METRICS.filter(m => m.showDetails).forEach(m => {
            const effectiveId = `${m.id}${effectiveMode.suffix}`;
            const values = computedGeoData.features
                .map((f: any) => f.properties?.[effectiveId])
                .filter((v: any) => v !== undefined && !isNaN(v) && !(m.ignoreValues?.includes(v)));
            if (values.length === 0) {
                result[m.id] = [0, 1];
                return;
            }

            const min = Math.min(...values);
            const max = Math.max(...values);

            if (m.quantiles && m.quantiles > 1) {
                const sorted = [...values].sort((a, b) => a - b);
                const n = m.quantiles;
                const quants = [];
                for (let i = 0; i <= n; i++) {
                    const idx = Math.min(Math.floor((i / n) * (sorted.length - 1)), sorted.length - 1);
                    quants.push(sorted[idx]);
                }
                result[m.id] = quants;
            } else {
                result[m.id] = [min, max];
            }
        });
        return result;
    }, [computedGeoData, effectiveMode]);

    const getColor = (val: number, domain: number[], metric: MetricDef) => {
        if (val === null || val === undefined || metric.ignoreValues?.includes(val)) return 'rgba(0,0,0,0.05)';
        const activeArray = metric.pallete;

        if (domain.length > 2) {
            // Use discrete scale breaks (quantiles/slices)
            let sliceIdx = 0;
            // Find the slice where the value falls
            for (let i = 0; i < domain.length - 1; i++) {
                if (val >= domain[i] && val <= domain[i + 1]) {
                    sliceIdx = i;
                    break;
                }
            }
            // Ensure sliceIdx stays within bounds of the palette mapping
            const nSlices = domain.length - 1;
            const paletteIdx = Math.min(Math.floor((sliceIdx / nSlices) * activeArray.length), activeArray.length - 1);
            return activeArray[paletteIdx];
        } else {
            // Use continuous linear interpolation mapping
            const [min, max] = domain;
            const range = max - min;
            if (range === 0) return activeArray[Math.floor(activeArray.length / 2)];
            const norm = Math.max(0, Math.min(1, (val - min) / range));
            const idx = Math.min(Math.floor(norm * activeArray.length), activeArray.length - 1);
            return activeArray[idx];
        }
    };

    const getLegendGradient = () => {
        const activeArray = selectedMetric.pallete;
        if (currentDomain.length > 2) {
            const nSlices = currentDomain.length - 1;
            const gradientParts = [];
            for (let i = 0; i < nSlices; i++) {
                const paletteIdx = Math.min(Math.floor((i / nSlices) * activeArray.length), activeArray.length - 1);
                const color = activeArray[paletteIdx];
                const start = (i / nSlices) * 100;
                const end = ((i + 1) / nSlices) * 100;
                gradientParts.push(`${color} ${start}% ${end}%`);
            }
            return `linear-gradient(to right, ${gradientParts.join(', ')})`;
        }
        // For sequential colormaps like Viridis, we pick 5 equidistant stops to ensure a faithful gradient representation in CSS
        const stops = [0, Math.floor(activeArray.length * 0.25), Math.floor(activeArray.length * 0.5), Math.floor(activeArray.length * 0.75), activeArray.length - 1];
        let colors = stops.map(i => activeArray[Math.min(i, activeArray.length - 1)]);
        return `linear-gradient(to right, ${colors.join(', ')})`;
    };

    const getStyle = (feature: any) => {
        const effectiveId = `${selectedMetric.id}${effectiveMode.suffix}`;
        const val = feature.properties[effectiveId];
        const isSelected = selectedFeature && feature.properties.id === selectedFeature.id;
        return {
            fillColor: getColor(val || 0, currentDomain, selectedMetric),
            weight: isSelected ? 3 : (effectiveLevel === 'hex' ? 0.3 : 0.6),
            opacity: 1,
            color: isSelected ? '#a6a6a6' : 'white',
            fillOpacity: isSelected ? 3 : 0.75,
            dashArray: (effectiveLevel === 'freguesia' && !isSelected) ? '3' : '',
        };
    };

    const onEachFeature = (feature: any, layer: any) => {
        const props = feature.properties;
        const effectiveId = `${selectedMetric.id}${effectiveMode.suffix}`;
        const val = props[effectiveId] ?? props[selectedMetric.id];
        const formattedVal = selectedMetric.format(val || 0, currentDomain[0], currentDomain[currentDomain.length - 1]);

        const parentLevel = LEVEL_CONFIG[effectiveLevel].parent;
        const parentName = (parentLevel && props.group_id)
            ? (dataState.parentLookup[`${parentLevel}-${props.group_id}`] || props.group_id)
            : '';

        const metricColor = getColor(val || 0, currentDomain, selectedMetric);
        const getContrastColor = (hexcolor: string) => {
            const r = parseInt(hexcolor.slice(1, 3), 16);
            const g = parseInt(hexcolor.slice(3, 5), 16);
            const b = parseInt(hexcolor.slice(5, 7), 16);
            const yiq = ((r * 299) + (g * 587) + (b * 114)) / 1000;
            return (yiq >= 128) ? '#111' : '#fff';
        };
        const contrastColor = getContrastColor(metricColor);

        const isIgnored = selectedMetric.ignoreValues?.includes(val);

        layer.bindTooltip(`
            <div style="font-family: sans-serif; padding: 6px; min-width: 140px;">
                <div style="font-size: 10px; font-weight: 900; color: #888; text-transform: uppercase; margin-bottom: 2px;">${parentName}</div>
                <div style="font-size: 13px; font-weight: 800; color: #111; line-height: 1.2;">${props.name || 'N/A'}</div>
                ${!isIgnored ? `
                <div style="margin-top: 10px; display: flex; align-items: center; justify-content: space-between; gap: 12px;">
                    <span style="font-size: 10px; font-weight: 900; color: #444; text-transform: uppercase;">${t(selectedMetric.label)}</span>
                    <span style="font-size: 11px; font-weight: 900; background-color: ${metricColor}; color: ${contrastColor}; padding: 3px 10px; border-radius: 20px; box-shadow: 0 1px 2px rgba(0,0,0,0.1); white-space: nowrap;">
                        ${formattedVal}${selectedMetric.unit ? ' ' + selectedMetric.unit : ''}
                    </span>
                </div>
                ` : `<div style="margin-top: 8px; font-size: 9px; font-weight: 900; color: #bbb; text-transform: uppercase;">${t('common.no_data')}</div>`}
            </div>
        `, { sticky: true, opacity: 0.98, direction: 'top', offset: [0, -10] });

        layer.on({
            click: (e: L.LeafletMouseEvent) => {
                L.DomEvent.stopPropagation(e);
                setSelectedFeature(props);
            }
        });
    };

    const toggleSection = (cat: string) => {
        setCollapsedSections(prev => ({
            ...prev,
            [cat]: !prev[cat]
        }));
    };

    const subLevelData = useMemo(() => {
        if (!selectedFeature) return [];
        const childLevel = (Object.keys(LEVEL_CONFIG) as ViewLevel[]).find(l => LEVEL_CONFIG[l].parent === effectiveLevel);
        if (!childLevel || !dataState.geo[childLevel]) return [];
        const effectiveId = `${selectedMetric.id}${effectiveMode.suffix}`;
        return dataState.geo[childLevel].features
            .filter((f: any) => String(f.properties.group_id) === String(selectedFeature.id))
            .map((f: any) => f.properties)
            .filter((p: any) => {
                const v = p[effectiveId] ?? p[selectedMetric.id];
                return v !== undefined && !(selectedMetric.ignoreValues?.includes(v));
            })
            .sort((a: any, b: any) => (b[effectiveId] ?? b[selectedMetric.id]) - (a[effectiveId] ?? a[selectedMetric.id]));
    }, [effectiveLevel, selectedFeature, selectedMetric, effectiveMode, dataState]);

    const chartData = useMemo(() => {
        if (!computedGeoData?.features) return { bestPerformers: [], worstPerformers: [] };
        const effectiveId = `${selectedMetric.id}${effectiveMode.suffix}`;
        const parentLevel = LEVEL_CONFIG[effectiveLevel].parent;
        const feats = computedGeoData.features
            .filter((f: any) => {
                const val = f.properties?.[effectiveId] ?? f.properties?.[selectedMetric.id];
                return val !== undefined && !(selectedMetric.ignoreValues?.includes(val));
            })
            .map((f: any) => {
                const val = f.properties?.[effectiveId] ?? f.properties?.[selectedMetric.id] ?? 0;
                const groupName = (parentLevel && f.properties?.group_id)
                    ? (dataState.parentLookup[`${parentLevel}-${f.properties.group_id}`] || f.properties.group_id)
                    : 'LMA';
                return {
                    id: f.properties?.id,
                    name: String(f.properties?.name || f.properties?.id || 'Unknown'),
                    group: groupName,
                    value: val,
                    color: getColor(val, currentDomain, selectedMetric)
                };
            });
        const sorted = [...feats].sort((a, b) => b.value - a.value);
        return { bestPerformers: sorted.slice(0, 5), worstPerformers: [...sorted].reverse().slice(0, 5).reverse() };
    }, [computedGeoData, selectedMetric, effectiveMode, currentDomain, getColor, effectiveLevel, dataState.parentLookup]);

    const resetWeights = () => {
        setWeights(defaultWeights);
    };

    if (dataState.loading) return (
        <div className={`h-screen w-screen ${isDarkMode ? 'bg-neutral-950' : 'bg-neutral-50'} flex flex-col items-center justify-center gap-4`}>
            <Loader2 className="w-12 h-12 text-sky-800 animate-spin" />
            <p className={`font-black uppercase tracking-[0.2em] text-[12px] ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>{t('common.loading')}</p>
        </div>
    );

    return (
        <div className={`relative h-screen w-screen ${isDarkMode ? 'bg-neutral-950 text-neutral-100' : 'bg-neutral-50 text-neutral-900'} font-sans overflow-hidden transition-colors duration-300`}>

            {!isMobile && (
                <SidebarLeft
                    isDarkMode={isDarkMode}
                    setIsDarkMode={setIsDarkMode}
                    setShowDownload={setShowDownload}
                    setShowAbout={setShowAbout}
                    selectedMetric={selectedMetric}
                    selectedMetricId={selectedMetricId}
                    setSelectedMetricId={setSelectedMetricId}
                    viewLevel={effectiveLevel}
                    collapsedSections={collapsedSections}
                    toggleSection={toggleSection}
                    weights={weights}
                    setWeights={setWeights}
                    resetWeights={resetWeights}
                    setIsAHPModalOpen={setIsAHPModalOpen}
                />
            )}

            {/* Map Canvas: Main View */}
            <div className="absolute inset-0 bg-neutral-950 z-0">
                <style>{`.leaflet-container { outline: none !important; } .leaflet-path { cursor: pointer; outline: none !important; }`}</style>

                {!isMobile && (
                    <div className="absolute top-8 left-[416px] z-[1002] flex items-start pointer-events-none">
                        <div className="flex gap-4 pointer-events-auto items-center">
                            <MapFilterDropdown
                                label={t('map.view_level')}
                                value={viewLevel}
                                isDark={isDarkMode}
                                icon={<Layers className="w-3.5 h-3.5" />}
                                options={(['hex', 'freguesia', 'municipality'] as const)
                                    .filter(l => isMetricAvailable(selectedMetricId, l, effectiveMode.suffix))
                                    .map(l => ({ id: l, label: l === 'hex' ? t('map.grid') : t(`map.${l}`) }))}
                                onChange={(id) => {
                                    setViewLevel(id as ViewLevel);
                                    setZoomRequest(null);
                                    setSelectedFeature(null);
                                }}
                            />

                            <MapFilterDropdown
                                label={t('map.region')}
                                value={nutFilter}
                                isDark={isDarkMode}
                                icon={<Globe className="w-3.5 h-3.5" />}
                                options={REGION_KEYS.map(n => ({ id: n, label: t(REGIONS[n].name) }))}
                                onChange={(id) => {
                                    setNutFilter(id as RegionKey);
                                    setZoomRequest(null);
                                    setSelectedFeature(null);
                                }}
                            />

                            <MapFilterDropdown
                                label={t('map.mode')}
                                value={effectiveMode.id}
                                isDark={isDarkMode}
                                icon={<RocketIcon className="w-3.5 h-3.5" />}
                                options={MODES.filter(m => isModeAvailable(m.id, selectedMetricId, effectiveLevel))
                                    .map(m => ({ id: m.id, label: t(m.label), icon: m.icon }))}
                                onChange={(id) => setSelectedModeId(id as ModeId)}
                            />
                        </div>
                    </div>
                )}

                {!isMobile && (
                    <div className="absolute bottom-8 right-8 z-[1000] pointer-events-none w-[280px]">
                        <div className={`p-6 rounded-[32px] border pointer-events-auto shadow-2xl backdrop-blur-xl ${isDarkMode ? 'bg-neutral-900/90 border-neutral-800' : 'bg-white/90 border-neutral-100'}`}>
                            <h4 className="flex items-center gap-2.5 text-[12px] font-black text-sky-800 mb-5 uppercase tracking-[0.1em]">
                                <Activity className="w-3.5 h-3.5" /> {nutFilter !== REGION_KEYS[0] ? t('map.local_rescaling') : t('map.global_scale')}
                            </h4>
                            <div className="space-y-5">
                                <div className="flex flex-col gap-3">
                                    <div
                                        className="h-2.5 rounded-full w-full shadow-inner"
                                        style={{ background: getLegendGradient() }}
                                    />
                                    <div className="flex justify-between text-[13px] font-black opacity-40 uppercase tracking-tighter">
                                        <span>{selectedMetric.format(currentDomain[0], currentDomain[0], currentDomain[currentDomain.length - 1])}</span>
                                        <span>{selectedMetric.format(currentDomain[currentDomain.length - 1], currentDomain[0], currentDomain[currentDomain.length - 1])}</span>
                                    </div>
                                </div>
                                <div className={`pt-4 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-200'}`}>
                                    <p className="text-[13px] font-black leading-tight mb-2 uppercase tracking-tight">{t(selectedMetric.label)} {selectedMetric.unit ? `(${selectedMetric.unit})` : ''}</p>
                                    <p className="text-[13px] opacity-40 leading-relaxed font-bold tracking-tight">{selectedMetric.description ? t(selectedMetric.description) : `Spatial distribution and variance of ${t(selectedMetric.label).toLowerCase()} across the ${t(`map.${effectiveLevel}`)} network.`}</p>
                                </div>
                            </div>
                        </div>
                    </div>
                )}

                <div className="absolute inset-0">
                    <MapContainer center={[38.74, -9.14]} zoom={11} className="h-full w-full" zoomControl={false} style={{ background: isDarkMode ? '#0a0a0a' : '#f0f0f0' }}>
                        <ZoomHandler extent={nutFilter === REGION_KEYS[0] ? DEFAULT_REGION : nutFilter} />
                        <SelectedFeatureCentering zoomRequest={zoomRequest} activeGeoData={computedGeoData} />
                        <MapDeselectHandler onDeselect={() => setSelectedFeature(null)} />
                        {(() => {
                            const layer = MAP_LAYERS.find(l => l.id === mapStyle) || MAP_LAYERS[0];
                            const url = 'getUrl' in layer ? layer.getUrl(isDarkMode) : layer.url;
                            return <TileLayer url={url!} attribution={layer.attribution} />;
                        })()}
                        <MapTools isDarkMode={isDarkMode} mapStyle={mapStyle} setMapStyle={setMapStyle} />
                        {computedGeoData?.features && (
                            <GeoJSON key={`${effectiveLevel}-${nutFilter}-${selectedMetricId}-${effectiveMode.id}-${isDarkMode}-${selectedFeature?.id}-${i18n.language}-${JSON.stringify(weights)}`} data={computedGeoData as any} style={getStyle} onEachFeature={onEachFeature} />
                        )}
                        <Pane name="limits-pane" style={{ zIndex: 450 }}>
                            {effectiveLevel !== 'municipality' && filteredLimits && (
                                <GeoJSON key={`limits-${nutFilter}-${isDarkMode}`} data={filteredLimits as any} style={{ fillOpacity: 0, weight: 4, color: isDarkMode ? 'rgba(255,255,255,0.4)' : 'rgba(0,0,0,0.3)' }} interactive={false} />
                            )}
                        </Pane>
                    </MapContainer>
                </div>
            </div>

            {!isMobile && (
                <SidebarRight
                    isDarkMode={isDarkMode}
                    selectedFeature={selectedFeature}
                    viewLevel={effectiveLevel}
                    selectedMetric={selectedMetric}
                    selectedMetricId={selectedMetricId}
                    selectedMode={effectiveMode}
                    dataState={dataState}
                    allDomains={allDomains}
                    getColor={getColor}
                    subLevelData={subLevelData}
                    chartData={chartData}
                    setSelectedFeature={setSelectedFeature}
                    computedGeoData={computedGeoData}
                    setZoomRequest={setZoomRequest}
                    setViewLevel={setViewLevel}
                />
            )}

            <AboutModal
                showAbout={showAbout}
                setShowAbout={setShowAbout}
                isDarkMode={isDarkMode}
            />

            <DownloadModal
                showDownload={showDownload}
                setShowDownload={setShowDownload}
                isDarkMode={isDarkMode}
                dataState={dataState}
            />

            <AHPModal
                isOpen={isAHPModalOpen}
                onClose={() => setIsAHPModalOpen(false)}
                metrics={contributoryMetrics}
                isDarkMode={isDarkMode}
                onApplyWeights={(newWeights: Record<string, number>) => {
                    setWeights(newWeights);
                    setIsAHPModalOpen(false);
                }}
            />

            {isMobile && !showAbout && (
                <MobileOverlay onShowAbout={() => setShowAbout(true)} />
            )}
        </div >
    );
};

export default Dashboard;
