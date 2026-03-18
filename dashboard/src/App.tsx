import { useState, useMemo, useEffect } from 'react';
import { MapContainer, TileLayer, GeoJSON, Pane } from 'react-leaflet';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import { Loader2, AlertTriangle, Activity, Layers, Globe, Zap, RocketIcon } from 'lucide-react';

import { ViewLevel, MetricDef } from './types';
import { METRICS, FLAT_METRICS, COLORS, REGION_KEYS, REGIONS, DEFAULT_REGION, MODES, RegionKey, ModeId, LEVEL_CONFIG } from './constants';
import { ZoomHandler, SelectedFeatureCentering, MapDeselectHandler } from './components/MapHandlers';
import { AHPModal } from './components/AHPModal';
import { SidebarLeft } from './components/SidebarLeft';
import { SidebarRight } from './components/SidebarRight';
import { AboutModal } from './components/AboutModal';
import { DownloadModal } from './components/DownloadModal';
import { MapFilterDropdown } from './components/MapFilterDropdown';

const Dashboard = () => {
    const [viewLevel, setViewLevel] = useState<ViewLevel>('freguesia');
    const [nutFilter, setNutFilter] = useState<RegionKey>(DEFAULT_REGION);
    const [selectedMetricId, setSelectedMetricId] = useState<string>(FLAT_METRICS.find(m => m.default)?.id || FLAT_METRICS[0].id);
    const [selectedModeId, setSelectedModeId] = useState<ModeId>('all');
    const [selectedFeature, setSelectedFeature] = useState<any>(null);
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
        if (modeId === 'all') return true;

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
        const effectiveId = `${checkMetricId}${mode.suffix}`;
        return !!(feature?.properties && feature.properties[effectiveId] !== undefined);
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
        const effectiveId = `${checkMetricId}${modeSuffix}`;
        return feature && feature.properties && (feature.properties[effectiveId] !== undefined || feature.properties[checkMetricId] !== undefined);
    };

    // Auto-switch view level OR reset mode if not available for selected metric
    useEffect(() => {
        // Reset mode if current mode is not available for selected metric at this level
        if (selectedModeId !== 'all' && !isModeAvailable(selectedModeId, selectedMetricId, viewLevel)) {
            setSelectedModeId('all');
            return;
        }

        if (!isMetricAvailable(selectedMetricId, viewLevel)) {
            const availableLevel = (['hex', 'freguesia', 'municipality'] as const).find(l => isMetricAvailable(selectedMetricId, l));
            if (availableLevel) {
                setViewLevel(availableLevel);
            }
        }
    }, [selectedMetricId, selectedModeId, viewLevel, dataState.geo]);

    const computedGeoData = useMemo(() => {
        let raw = dataState.geo[viewLevel];
        if (!raw || !raw.features) return { type: "FeatureCollection", features: [] };

        let features = raw.features;
        if (nutFilter !== REGION_KEYS[0]) {
            features = features.filter((f: any) => f.properties?.region_id === nutFilter || f.properties?.nuts2 === REGIONS[nutFilter].name);
        }

        // Inject dynamic IMPT metric computed on-the-fly based on user weights
        const dynamicMetricConfig = FLAT_METRICS.find(m => m.isCalculated);
        if (dynamicMetricConfig) {
            const dynamicId = `${dynamicMetricConfig.id}${selectedMode.suffix}`;

            features = features.map((f: any) => {
                let computedIndex = 0;

                Object.entries(weights).forEach(([metricId, weight]) => {
                    const effectiveMetricId = `${metricId}${selectedMode.suffix}`;
                    const val = f.properties[effectiveMetricId] ?? 0;
                    computedIndex += val * weight;
                });

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
    }, [viewLevel, nutFilter, dataState, weights, selectedMode.suffix]);
    const filteredLimits = useMemo(() => {
        if (!dataState.limits) return null;
        if (nutFilter === REGION_KEYS[0]) return dataState.limits;

        return {
            ...dataState.limits,
            features: dataState.limits.features.filter((f: any) => f.properties?.region_id === nutFilter)
        };
    }, [dataState.limits, nutFilter]);

    const currentDomain = useMemo(() => {
        const defaultDomain: [number, number] = [0, 1];
        if (!computedGeoData?.features?.length) return defaultDomain;
        const effectiveId = `${selectedMetric.id}${selectedMode.suffix}`;
        const values = computedGeoData.features.map((f: any) => f.properties?.[effectiveId]).filter((v: any) => v !== undefined && !isNaN(v));
        if (values.length === 0) return defaultDomain;
        return [Math.min(...values), Math.max(...values)] as [number, number];
    }, [computedGeoData, selectedMetric, selectedMode]);

    // Precompute domains for all metrics shown in details to support dynamic coloring
    const allDomains = useMemo(() => {
        const result: Record<string, [number, number]> = {};
        if (!computedGeoData?.features?.length) return result;
        FLAT_METRICS.filter(m => m.showDetails).forEach(m => {
            const effectiveId = `${m.id}${selectedMode.suffix}`;
            const values = computedGeoData.features.map((f: any) => f.properties?.[effectiveId]).filter((v: any) => v !== undefined && !isNaN(v));
            result[m.id] = values.length > 0 ? [Math.min(...values), Math.max(...values)] : [0, 1];
        });
        return result;
    }, [computedGeoData, selectedMode]);

    const getColor = (val: number, domain: [number, number], metric: MetricDef) => {
        if (val === null || val === undefined) return '#333';
        const [min, max] = domain;
        const range = max - min;

        const seq = COLORS.Sequential;
        const dng = COLORS.Danger;
        const divNeg = COLORS.Divergent.negative;
        const divPos = COLORS.Divergent.positive;

        if (range === 0) return metric.higherTheBetter ? seq[Math.floor(seq.length / 2)] : dng[Math.floor(dng.length / 2)];

        if (metric.isDivergent) {
            const mid = (min + max) / 2;
            if (val <= mid) {
                const subNorm = (val - min) / (mid - min || 1);
                const idx = Math.min(Math.floor(subNorm * divNeg.length), divNeg.length - 1);
                return divNeg[idx];
            } else {
                const subNorm = (val - mid) / (max - mid || 1);
                const idx = Math.min(Math.floor(subNorm * divPos.length), divPos.length - 1);
                return divPos[idx];
            }
        }

        const norm = Math.max(0, Math.min(1, (val - min) / range));
        const activeArray = metric.higherTheBetter ? seq : dng;
        const idx = Math.min(Math.floor(norm * activeArray.length), activeArray.length - 1);
        return activeArray[idx];
    };

    const getLegendGradient = () => {
        if (selectedMetric.isDivergent) {
            return `linear-gradient(to right, ${COLORS.Divergent.negative[0]}, #fff, ${COLORS.Divergent.positive[COLORS.Divergent.positive.length - 1]})`;
        }

        const activeArray = selectedMetric.higherTheBetter ? COLORS.Sequential : COLORS.Danger;
        // For sequential colormaps like Viridis, we pick 5 equidistant stops to ensure a faithful gradient representation in CSS
        const stops = [0, Math.floor(activeArray.length * 0.25), Math.floor(activeArray.length * 0.5), Math.floor(activeArray.length * 0.75), activeArray.length - 1];
        const colors = stops.map(i => activeArray[Math.min(i, activeArray.length - 1)]);
        return `linear-gradient(to right, ${colors.join(', ')})`;
    };

    const getStyle = (feature: any) => {
        const effectiveId = `${selectedMetric.id}${selectedMode.suffix}`;
        const val = feature.properties[effectiveId];
        const isSelected = selectedFeature && feature.properties.id === selectedFeature.id;
        return {
            fillColor: getColor(val || 0, currentDomain, selectedMetric),
            weight: isSelected ? 3 : (viewLevel === 'hex' ? 0.3 : 0.6),
            opacity: 1,
            color: isSelected ? '#a5b4fc' : 'white',
            fillOpacity: isSelected ? 0.9 : 0.75,
            dashArray: (viewLevel === 'freguesia' && !isSelected) ? '3' : ''
        };
    };

    const onEachFeature = (feature: any, layer: any) => {
        const props = feature.properties;
        const effectiveId = `${selectedMetric.id}${selectedMode.suffix}`;
        const val = props[effectiveId] ?? props[selectedMetric.id];
        const formattedVal = selectedMetric.format(val || 0);

        const parentLevel = LEVEL_CONFIG[viewLevel].parent;
        const parentName = (parentLevel && props.group_id)
            ? (dataState.parentLookup[`${parentLevel}-${props.group_id}`] || props.group_id)
            : 'LMA';

        layer.bindTooltip(`
            <div style="font-family: sans-serif; padding: 4px;">
                <div style="font-size: 10px; font-weight: 900; color: #666; text-transform: uppercase;">${parentName}</div>
                <div style="font-size: 12px; font-weight: 700; color: #111;">${props.name || 'N/A'}</div>
                <div style="font-size: 11px; font-weight: 900; color: ${getColor(val || 0, currentDomain, selectedMetric)}; margin-top: 4px;">${selectedMetric.label}: ${formattedVal} ${selectedMetric.unit || ''}</div>
            </div>
        `, { sticky: true, opacity: 0.95 });

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
        if (!selectedFeature || viewLevel === 'hex') return [];
        const level = viewLevel === 'municipality' ? 'freguesia' : 'hex';
        if (!dataState.geo[level]) return [];
        const effectiveId = `${selectedMetric.id}${selectedMode.suffix}`;
        return dataState.geo[level].features
            .filter((f: any) => String(f.properties.group_id) === String(selectedFeature.id))
            .map((f: any) => f.properties)
            .sort((a: any, b: any) => ((b[effectiveId] ?? b[selectedMetric.id]) || 0) - ((a[effectiveId] ?? a[selectedMetric.id]) || 0));
    }, [viewLevel, selectedFeature, selectedMetric, selectedMode, dataState]);

    const chartData = useMemo(() => {
        if (!computedGeoData?.features) return { top10: [], worst10: [] };
        const effectiveId = `${selectedMetric.id}${selectedMode.suffix}`;
        const parentLevel = LEVEL_CONFIG[viewLevel].parent;
        const feats = computedGeoData.features.map((f: any) => {
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
        return { top10: sorted.slice(0, 10), worst10: [...sorted].reverse().slice(0, 10).reverse() };
    }, [computedGeoData, selectedMetric, selectedMode, currentDomain, getColor, viewLevel, dataState.parentLookup]);

    const resetWeights = () => {
        setWeights(defaultWeights);
    };

    if (dataState.loading) return (
        <div className={`h-screen w-screen ${isDarkMode ? 'bg-neutral-950' : 'bg-neutral-50'} flex flex-col items-center justify-center gap-4`}>
            <Loader2 className="w-12 h-12 text-sky-800 animate-spin" />
            <p className={`font-black uppercase tracking-[0.2em] text-[12px] ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>Loading IMPT data...</p>
        </div>
    );

    return (
        <div className={`relative h-screen w-screen ${isDarkMode ? 'bg-neutral-950 text-neutral-100' : 'bg-neutral-50 text-neutral-900'} font-sans overflow-hidden transition-colors duration-300`}>

            <SidebarLeft
                isDarkMode={isDarkMode}
                setIsDarkMode={setIsDarkMode}
                setShowDownload={setShowDownload}
                setShowAbout={setShowAbout}
                selectedMetric={selectedMetric}
                selectedMetricId={selectedMetricId}
                setSelectedMetricId={setSelectedMetricId}
                viewLevel={viewLevel}
                collapsedSections={collapsedSections}
                toggleSection={toggleSection}
                weights={weights}
                setWeights={setWeights}
                resetWeights={resetWeights}
                setIsAHPModalOpen={setIsAHPModalOpen}
            />

            {/* Map Canvas: Main View */}
            <div className="absolute inset-0 bg-neutral-950 z-0">
                <style>{`.leaflet-container { outline: none !important; } .leaflet-path { cursor: pointer; outline: none !important; }`}</style>

                {selectedMetric.isFake && (
                    <div className="absolute top-[88px] left-[416px] z-[1000] flex items-center gap-2 px-3 py-1.5 rounded-lg bg-red-600/90 text-white shadow-xl backdrop-blur-md animate-pulse">
                        <AlertTriangle className="w-3.5 h-3.5" />
                        <span className="text-[13px] font-black uppercase tracking-widest">Synthetic / Placeholder Data</span>
                    </div>
                )}

                <div className="absolute top-8 left-[416px] z-[1002] flex items-start pointer-events-none">
                    <div className="flex gap-4 pointer-events-auto items-center">
                        <MapFilterDropdown
                            label="View Level"
                            value={viewLevel}
                            isDark={isDarkMode}
                            icon={<Layers className="w-3.5 h-3.5" />}
                            options={(['hex', 'freguesia', 'municipality'] as const)
                                .filter(l => isMetricAvailable(selectedMetricId, l))
                                .map(l => ({ id: l, label: l === 'hex' ? 'Grid' : l }))}
                            onChange={(id) => setViewLevel(id as ViewLevel)}
                        />

                        <MapFilterDropdown
                            label="Region"
                            value={nutFilter}
                            isDark={isDarkMode}
                            icon={<Globe className="w-3.5 h-3.5" />}
                            options={REGION_KEYS.map(n => ({ id: n, label: REGIONS[n].name }))}
                            onChange={(id) => setNutFilter(id as RegionKey)}
                        />

                        <MapFilterDropdown
                            label="Mode"
                            value={selectedModeId}
                            isDark={isDarkMode}
                            icon={<RocketIcon className="w-3.5 h-3.5" />}
                            options={MODES.filter(m => isModeAvailable(m.id, selectedMetricId, viewLevel))
                                .map(m => ({ id: m.id, label: m.label, icon: m.icon }))}
                            onChange={(id) => setSelectedModeId(id as ModeId)}
                        />
                    </div>
                </div>

                <div className="absolute bottom-8 right-8 z-[1000] pointer-events-none w-[280px]">
                    <div className={`p-6 rounded-[32px] border pointer-events-auto shadow-2xl backdrop-blur-xl ${isDarkMode ? 'bg-neutral-900/90 border-neutral-800' : 'bg-white/90 border-neutral-100'}`}>
                        <h4 className="flex items-center gap-2.5 text-[12px] font-black text-sky-800 mb-5 uppercase tracking-[0.1em]">
                            <Activity className="w-3.5 h-3.5" /> {nutFilter !== REGION_KEYS[0] ? 'Local Rescaling' : 'Global Metric Scale'}
                        </h4>
                        <div className="space-y-5">
                            <div className="flex flex-col gap-3">
                                <div
                                    className="h-2.5 rounded-full w-full shadow-inner"
                                    style={{ background: getLegendGradient() }}
                                />
                                <div className="flex justify-between text-[13px] font-black opacity-40 uppercase tracking-tighter">
                                    <span>{selectedMetric.format(currentDomain[0])}</span>
                                    <span>{selectedMetric.format(currentDomain[1])}</span>
                                </div>
                            </div>
                            <div className={`pt-4 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-200'}`}>
                                <p className="text-[13px] font-black leading-tight mb-2 uppercase tracking-tight">{selectedMetric.label} {selectedMetric.unit ? `(${selectedMetric.unit})` : ''}</p>
                                <p className="text-[13px] opacity-40 leading-relaxed font-bold tracking-tight">{selectedMetric.description || `Spatial distribution and variance of ${selectedMetric.label.toLowerCase()} across the ${viewLevel} network.`}</p>
                            </div>
                        </div>
                    </div>
                </div>

                <div className="absolute inset-0">
                    <MapContainer center={[38.74, -9.14]} zoom={11} className="h-full w-full" zoomControl={false} style={{ background: isDarkMode ? '#0a0a0a' : '#f0f0f0' }}>
                        <ZoomHandler extent={nutFilter === REGION_KEYS[0] ? DEFAULT_REGION : nutFilter} />
                        <SelectedFeatureCentering zoomRequest={zoomRequest} activeGeoData={computedGeoData} />
                        <MapDeselectHandler onDeselect={() => setSelectedFeature(null)} />
                        <TileLayer url={isDarkMode ? "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png" : "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"} attribution='&copy; CARTO' />
                        {computedGeoData?.features && (
                            <GeoJSON key={`${viewLevel}-${nutFilter}-${selectedMetricId}-${selectedModeId}-${isDarkMode}-${selectedFeature?.id}-${JSON.stringify(weights)}`} data={computedGeoData as any} style={getStyle} onEachFeature={onEachFeature} />
                        )}
                        <Pane name="limits-pane" style={{ zIndex: 450 }}>
                            {viewLevel !== 'municipality' && filteredLimits && (
                                <GeoJSON key={`limits-${nutFilter}-${isDarkMode}`} data={filteredLimits as any} style={{ fillOpacity: 0, weight: 4, color: isDarkMode ? 'rgba(255,255,255,0.4)' : 'rgba(0,0,0,0.3)' }} interactive={false} />
                            )}
                        </Pane>
                    </MapContainer>
                </div>
            </div>

            <SidebarRight
                isDarkMode={isDarkMode}
                selectedFeature={selectedFeature}
                viewLevel={viewLevel}
                selectedMetric={selectedMetric}
                selectedMetricId={selectedMetricId}
                selectedMode={selectedMode}
                dataState={dataState}
                allDomains={allDomains}
                getColor={getColor}
                subLevelData={subLevelData}
                chartData={chartData}
                setSelectedFeature={setSelectedFeature}
                computedGeoData={computedGeoData}
                setZoomRequest={setZoomRequest}
            />

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
        </div >
    );
};

export default Dashboard;
