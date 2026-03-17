
import { useState, useMemo, useEffect } from 'react';
import { MapContainer, TileLayer, GeoJSON, useMap } from 'react-leaflet';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import { BarChart, Bar, XAxis, YAxis, Tooltip as RechartsTooltip, ResponsiveContainer, Cell, LabelList, PieChart, Pie, Legend as RechartsLegend } from 'recharts';
import { Info, MapPin, ChevronRight, X, Github, Activity, Loader2, MousePointer2, ChevronDown, Sun, Moon, ExternalLink, ListFilter, TrendingUp, AlertTriangle, Download } from 'lucide-react';

// --- Types & Constants ---
type ViewLevel = 'hex' | 'freguesia' | 'municipality';

type MetricDef = {
    id: string;
    label: string;
    category: string;
    icon: string;
    description?: string;
    format: (v: number) => string;
    isDivergent?: boolean;
    higherTheBetter?: boolean;
    unit?: string;
    viewLevel?: 'municipality' | 'all'; // Restrict visibility
    isFake?: boolean; // Label as placeholder data
};

const METRICS: Record<string, MetricDef[]> = {
    'Mobility Poverty Index': [
        { id: 'IMPT_score_pca_avg', label: 'PCA + Average', category: 'Mobility Poverty Index', icon: '📈', description: 'Composite index (0-100) based on accessibility, car share, and vehicle ownership.', format: (v) => (v || 0).toFixed(1), higherTheBetter: true },
        { id: 'IMPT_Equal_Weights_avg', label: 'Equal Weights', category: 'Mobility Poverty Index', icon: '⚖️', description: 'Simple average of all base indicators normalized to 0-100 scale.', format: (v) => (v || 0).toFixed(1), higherTheBetter: true },
    ],
    'Accessibility': [
        { id: 'access_health_walk_60min_residents', label: 'Health (60m Walk)', category: 'Accessibility', icon: '🏥', format: (v) => `${(v || 0).toFixed(0)}`, higherTheBetter: true, unit: 'opp' },
        { id: 'access_groceries_walk_15min_residents', label: 'Groceries (15m Walk)', category: 'Accessibility', icon: '🛒', format: (v) => `${(v || 0).toFixed(0)}`, higherTheBetter: true, unit: 'opp' },
        { id: 'access_greenspaces_walk_15min_residents', label: 'Parks (15m Walk)', category: 'Accessibility', icon: '🌳', format: (v) => `${(v || 0).toFixed(0)}`, higherTheBetter: true, unit: 'opp' },
    ],
    'Mobility': [
        { id: 'mobility_cost_health_walk_n1_residents', label: 'Travel Time Health (Walk)', category: 'Mobility', icon: '⏱️', format: (v) => `${(v || 0).toFixed(0)}`, higherTheBetter: false, unit: 'min' },
        { id: 'mobility_cost_jobs_car_n1_active', label: 'Travel Time Work (Car)', category: 'Mobility', icon: '🚗', format: (v) => `${(v || 0).toFixed(0)}`, higherTheBetter: false, unit: 'min' },
        { id: 'mobility_transit_total_frequency_peak', label: 'Transit Frequency (Peak)', category: 'Mobility', icon: '🚌', format: (v) => `${(v || 0).toFixed(0)}`, higherTheBetter: true, unit: 'trips/h' },
    ],
    'Sociodemographic': [
        { id: 'census_residents', label: 'Total Residents', category: 'Sociodemographic', icon: '👥', format: (v) => (v || 0).toLocaleString(), higherTheBetter: true },
        { id: 'census_families', label: 'Total Families', category: 'Sociodemographic', icon: '🏠', format: (v) => (v || 0).toLocaleString(), higherTheBetter: true },
    ]
};

const FLAT_METRICS = Object.values(METRICS).flat();


const COLORS = {
    Sequential: ['#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b'],
    Danger: ['#fff5f0', '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d'],
    Divergent: {
        negative: ['#006837', '#31a354', '#78c679', '#c2e699', '#f7fcb9'],
        positive: ['#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#f03b20', '#bd0026']
    }
};

const REGIONS = {
    'metropolis': { name: "Metropolis", center: [38.74, -9.14], zoom: 11 },
    'PT1B': { name: "Grande Lisboa", center: [38.85, -9.15], zoom: 11 },
    'PT1A': { name: "Península de Setúbal", center: [38.55, -9.05], zoom: 11 }
} as const;

type RegionKey = keyof typeof REGIONS;

const REGION_KEYS = Object.keys(REGIONS) as RegionKey[];
const DEFAULT_REGION: RegionKey = REGION_KEYS[0];

const ZoomHandler = ({ extent }: { extent: RegionKey }) => {
    const map = useMap();
    useEffect(() => {
        const config = REGIONS[extent];
        map.setView(config.center as L.LatLngExpression, config.zoom);
    }, [extent, map]);
    return null;
};

const Dashboard = () => {
    const [viewLevel, setViewLevel] = useState<ViewLevel>('freguesia');
    const [nutFilter, setNutFilter] = useState<RegionKey>(DEFAULT_REGION);
    const [selectedMetricId, setSelectedMetricId] = useState<string>('IMPT_score_pca_avg');
    const [selectedFeature, setSelectedFeature] = useState<any>(null);
    const [showAbout, setShowAbout] = useState(false);
    const [showDownload, setShowDownload] = useState(false);
    const [isDarkMode, setIsDarkMode] = useState(false);
    const [collapsedSections, setCollapsedSections] = useState<Record<string, boolean>>(() => {
        const keys = Object.keys(METRICS);
        return keys.slice(1).reduce((acc, key) => ({ ...acc, [key]: true }), {});
    });

    const [dataState, setDataState] = useState<{
        geo: Record<string, any>; limits: any; loading: boolean; error: string | null;
    }>({ geo: {}, limits: null, loading: true, error: null });

    const LEVEL_FILES = {
        'municipality': 'data/municipios_aggregated.geojson',
        'freguesia': 'data/freguesias_aggregated.geojson',
        'hex': 'data/grid_aggregated.geojson'
    };

    useEffect(() => {
        const load = async () => {
            try {
                const levels = Object.keys(LEVEL_FILES);
                const results = await Promise.all([
                    ...levels.map(l => fetch(LEVEL_FILES[l as keyof typeof LEVEL_FILES]).then(r => r.json())),
                    fetch('data/municipios_limits.json').then(r => r.json())
                ]);

                const geo: Record<string, any> = {};
                levels.forEach((l, i) => { geo[l] = results[i]; });

                setDataState({ geo, limits: results[levels.length], loading: false, error: null });
            } catch (err) {
                console.error(err);
                setDataState(s => ({ ...s, loading: false, error: "System encountered a data loading error." }));
            }
        };
        load();
    }, []);

    const selectedMetric = useMemo(() => FLAT_METRICS.find(m => m.id === selectedMetricId) || FLAT_METRICS[0], [selectedMetricId]);

    // Helper to check if a metric is available at a certain view level
    const isMetricAvailable = (metricId: string, level: string) => {
        if (!dataState.geo[level as ViewLevel]) return false;
        // Check first feature to see if the property exists
        const feature = dataState.geo[level as ViewLevel]?.features[0];
        return feature && feature.properties && feature.properties[metricId] !== undefined;
    };

    // Auto-switch view level if not available for selected metric
    useEffect(() => {
        if (!isMetricAvailable(selectedMetricId, viewLevel)) {
            const availableLevel = (['hex', 'freguesia', 'municipality'] as const).find(l => isMetricAvailable(selectedMetricId, l));
            if (availableLevel) {
                setViewLevel(availableLevel);
            }
        }
    }, [selectedMetricId, dataState.geo]);

    const activeGeoData = useMemo(() => {
        let raw = dataState.geo[viewLevel];
        if (!raw || !raw.features) return { type: "FeatureCollection", features: [] };

        let features = raw.features;
        if (nutFilter !== REGION_KEYS[0]) {
            features = features.filter((f: any) => f.properties?.region_id === nutFilter || f.properties?.nuts2 === REGIONS[nutFilter].name);
        }
        return { ...raw, features };
    }, [viewLevel, nutFilter, dataState]);

    const currentDomain = useMemo(() => {
        const defaultDomain: [number, number] = [0, 1];
        if (!activeGeoData?.features?.length) return defaultDomain;
        const values = activeGeoData.features.map((f: any) => f.properties?.[selectedMetric.id]).filter((v: any) => v !== undefined && !isNaN(v));
        if (values.length === 0) return defaultDomain;
        return [Math.min(...values), Math.max(...values)] as [number, number];
    }, [activeGeoData, selectedMetric]);

    const getColor = (val: number, domain: [number, number], metric: MetricDef) => {
        if (val === null || val === undefined) return '#333';
        const [min, max] = domain;
        const range = max - min;
        if (range === 0) return metric.higherTheBetter ? COLORS.Sequential[4] : COLORS.Danger[4];

        if (metric.isDivergent) {
            const mid = (min + max) / 2;
            if (val <= mid) {
                const subNorm = (val - min) / (mid - min || 1);
                const idx = Math.min(Math.floor(subNorm * 5), 4);
                return COLORS.Divergent.negative[idx];
            } else {
                const subNorm = (val - mid) / (max - mid || 1);
                const idx = Math.min(Math.floor(subNorm * 6), 5);
                return COLORS.Divergent.positive[idx];
            }
        }

        const norm = Math.max(0, Math.min(1, (val - min) / range));
        const idx = Math.min(Math.floor(norm * 9), 8);
        return metric.higherTheBetter ? COLORS.Sequential[idx] : COLORS.Danger[idx];
    };

    const getStyle = (feature: any) => {
        const isSelected = selectedFeature && feature.properties.id === selectedFeature.id;
        return {
            fillColor: getColor(feature.properties[selectedMetric.id] || 0, currentDomain, selectedMetric),
            weight: isSelected ? 3 : (viewLevel === 'hex' ? 0.3 : 0.6),
            opacity: 1,
            color: isSelected ? '#a5b4fc' : 'white',
            fillOpacity: isSelected ? 0.9 : 0.75,
            dashArray: (viewLevel === 'freguesia' && !isSelected) ? '3' : ''
        };
    };

    const onEachFeature = (feature: any, layer: any) => {
        const props = feature.properties;
        const val = props[selectedMetric.id];
        const formattedVal = selectedMetric.format(val || 0);

        layer.bindTooltip(`
            <div style="font-family: sans-serif; padding: 4px;">
                <div style="font-size: 10px; font-weight: 900; color: #666; text-transform: uppercase;">${props.group_id || 'LMA'}</div>
                <div style="font-size: 12px; font-weight: 700; color: #111;">${props.name || 'N/A'}</div>
                <div style="font-size: 11px; font-weight: 900; color: #6366f1; margin-top: 4px;">${selectedMetric.label}: ${formattedVal} ${selectedMetric.unit || ''}</div>
            </div>
        `, { sticky: true, opacity: 0.95 });

        layer.on({
            click: () => {
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
        if (viewLevel !== 'municipality' || !selectedFeature || !dataState.geo['freguesia']) return [];
        return dataState.geo['freguesia'].features
            .filter((f: any) => String(f.properties?.group_id) === String(selectedFeature.id))
            .map((f: any) => f.properties)
            .sort((a: any, b: any) => (b[selectedMetric.id] || 0) - (a[selectedMetric.id] || 0));
    }, [viewLevel, selectedFeature, selectedMetric, dataState]);

    const chartData = useMemo(() => {
        if (!activeGeoData?.features) return { top10: [], worst10: [] };
        const feats = activeGeoData.features.map((f: any) => ({
            name: String(f.properties?.name || f.properties?.id || 'Unknown'),
            group: f.properties?.group_id || '',
            value: (f.properties?.[selectedMetric.id] || 0)
        }));
        const sorted = [...feats].sort((a, b) => b.value - a.value);
        return { top10: sorted.slice(0, 10), worst10: [...sorted].reverse().slice(0, 10).reverse() };
    }, [activeGeoData, selectedMetric]);

    if (dataState.loading) return (
        <div className={`h-screen w-screen ${isDarkMode ? 'bg-neutral-950' : 'bg-neutral-50'} flex flex-col items-center justify-center gap-4`}>
            <Loader2 className="w-12 h-12 text-indigo-500 animate-spin" />
            <p className={`font-black uppercase tracking-[0.2em] text-[10px] ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>Assembling Spatial Intelligence...</p>
        </div>
    );

    return (
        <div className={`flex h-screen ${isDarkMode ? 'bg-neutral-950 text-neutral-100' : 'bg-neutral-50 text-neutral-900'} font-sans overflow-hidden transition-colors duration-300`}>

            {/* Sidebar Left: Control Panel */}
            <div className={`w-[340px] flex flex-col ${isDarkMode ? 'bg-neutral-900 border-neutral-800' : 'bg-white border-neutral-200'} border-r shadow-xl z-30 transition-all`}>
                <div className={`p-7 border-b ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>
                    <div className="flex items-center gap-4 mb-6">
                        <img src="images/logo/logo.png" alt="Logo" className="w-12 h-12 object-contain" />
                        <div className="flex flex-col">
                            <h1 className="text-sm font-black tracking-tighter uppercase leading-none">Mobility <span className="text-indigo-500">Poverty</span></h1>
                            <p className="text-[10px] font-bold uppercase tracking-widest opacity-40 mt-1">Lisbon Metropolis</p>
                        </div>
                    </div>

                    <div className="flex items-center justify-between mb-4">
                        <div className="flex gap-1">
                            <button onClick={() => setIsDarkMode(!isDarkMode)} title="Toggle Theme" className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                {isDarkMode ? <Sun className="w-5 h-5" /> : <Moon className="w-5 h-5" />}
                            </button>
                            <button onClick={() => setShowDownload(true)} title="Download Data" className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                <Download className="w-5 h-5" />
                            </button>
                            <button onClick={() => setShowAbout(true)} title="About" className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                <Info className="w-5 h-5" />
                            </button>
                        </div>
                        <div className={`px-2.5 py-1 rounded-lg text-[9px] font-black uppercase tracking-widest ${isDarkMode ? 'bg-neutral-800 text-neutral-500' : 'bg-neutral-100 text-neutral-400'}`}>v3.7.0</div>
                    </div>
                </div>

                <div className="flex-1 overflow-y-auto p-6 space-y-8 scrollbar-hide">
                    <section>
                        <h3 className="text-[10px] font-black opacity-30 uppercase tracking-[0.3em] mb-4 flex items-center gap-2">
                            <ListFilter className="w-3 h-3 text-indigo-500" /> Indicator Space
                        </h3>
                        <div className="space-y-4">
                            {Object.keys(METRICS).map(cat => (
                                <div key={cat} className={`border rounded-2xl overflow-hidden transition-all ${isDarkMode ? 'border-neutral-800 bg-neutral-800/10' : 'border-neutral-100 bg-neutral-50/30'} ${!collapsedSections[cat] ? 'ring-1 ring-indigo-500/20' : ''}`}>
                                    <button onClick={() => toggleSection(cat)}
                                        className={`w-full flex items-center justify-between px-4 py-3.5 text-[10px] font-black uppercase tracking-widest transition-colors ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-600'}`}
                                    >
                                        <span className="flex items-center gap-2">
                                            <div className={`w-1.5 h-1.5 rounded-full ${selectedMetric.category === cat ? 'bg-indigo-500 shadow-[0_0_8px_rgba(99,102,241,0.5)]' : 'bg-neutral-700'}`} />{cat}
                                        </span>
                                        <ChevronDown className={`w-3.5 h-3.5 transition-transform duration-300 ${collapsedSections[cat] ? '' : 'rotate-180'}`} />
                                    </button>
                                    {!collapsedSections[cat] && (
                                        <div className={`p-2 space-y-1.5 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>
                                            {METRICS[cat].map(m => {
                                                const isVisible = !m.viewLevel || (m.viewLevel === 'municipality' && viewLevel === 'municipality');
                                                if (!isVisible) return null;
                                                return (
                                                    <button key={m.id} onClick={() => setSelectedMetricId(m.id)}
                                                        className={`w-full flex items-center justify-between px-3.5 py-2.5 rounded-xl text-[10px] font-bold transition-all ${selectedMetricId === m.id
                                                            ? 'bg-indigo-600 text-white shadow-lg'
                                                            : (isDarkMode ? 'hover:bg-neutral-800 text-neutral-500' : 'hover:bg-neutral-100 text-neutral-500')}`}
                                                    >
                                                        <span className="flex items-center gap-3">
                                                            <span>{m.icon}</span>
                                                            <span className="truncate">{m.label}</span>
                                                            {m.isFake && <AlertTriangle className="w-2.5 h-2.5 text-amber-500 ml-auto opacity-60" />}
                                                        </span>
                                                    </button>
                                                );
                                            })}
                                        </div>
                                    )}
                                </div>
                            ))}
                        </div>
                    </section>
                </div>
            </div>

            {/* Map Canvas: Main View */}
            <div className="flex-1 relative bg-neutral-950 flex flex-col">
                <style>{`.leaflet-container { outline: none !important; } .leaflet-path { cursor: pointer; outline: none !important; }`}</style>

                {selectedMetric.isFake && (
                    <div className="absolute top-[88px] left-8 z-[1000] flex items-center gap-2 px-3 py-1.5 rounded-lg bg-red-600/90 text-white shadow-xl backdrop-blur-md animate-pulse">
                        <AlertTriangle className="w-3.5 h-3.5" />
                        <span className="text-[9px] font-black uppercase tracking-widest">Synthetic / Placeholder Data</span>
                    </div>
                )}

                <div className="absolute top-8 left-8 right-8 z-[1000] flex justify-between items-start pointer-events-none">
                    <div className="flex gap-4 pointer-events-auto items-center">
                        <div className={`${isDarkMode ? 'bg-neutral-900/90 border-neutral-800 shadow-2xl' : 'bg-white/90 border-neutral-200 shadow-xl'} backdrop-blur-md px-1.5 py-1.5 rounded-2xl border flex items-center`}>
                            {(['hex', 'freguesia', 'municipality'] as const)
                                .filter(l => isMetricAvailable(selectedMetricId, l))
                                .map(l => (
                                    <button key={l} onClick={() => setViewLevel(l)}
                                        className={`px-5 py-2 rounded-xl text-[10px] font-black uppercase tracking-widest transition-all ${viewLevel === l ? 'bg-indigo-600 text-white shadow-xl' : `${isDarkMode ? 'text-neutral-500 hover:text-neutral-300' : 'text-neutral-400 hover:text-neutral-800'}`}`}
                                    >{l === 'hex' ? 'Grid' : l}</button>
                                ))}
                        </div>
                        <div className={`${isDarkMode ? 'bg-neutral-900/90 border-neutral-800 shadow-2xl' : 'bg-white/90 border-neutral-200 shadow-xl'} backdrop-blur-md px-1.5 py-1.5 rounded-2xl border flex items-center`}>
                            {REGION_KEYS.map(n => (
                                <button key={n} onClick={() => setNutFilter(n)}
                                    className={`px-5 py-2 rounded-xl text-[10px] font-black uppercase tracking-widest transition-all ${nutFilter === n ? 'bg-indigo-600 text-white shadow-xl' : `${isDarkMode ? 'text-neutral-500 hover:text-neutral-300' : 'text-neutral-400 hover:text-neutral-800'}`}`}
                                >{REGIONS[n].name}</button>
                            ))}
                        </div>
                    </div>
                </div>

                <div className="absolute bottom-8 left-8 z-[1000] pointer-events-none w-[280px]">
                    <div className={`p-6 rounded-[32px] border pointer-events-auto shadow-2xl backdrop-blur-xl ${isDarkMode ? 'bg-neutral-900/90 border-neutral-800' : 'bg-white/90 border-neutral-100'}`}>
                        <h4 className="flex items-center gap-2.5 text-[10px] font-black text-indigo-500 mb-5 uppercase tracking-[0.1em]">
                            <Activity className="w-3.5 h-3.5" /> {nutFilter !== REGION_KEYS[0] ? 'Local Rescaling' : 'Global Metric Scale'}
                        </h4>
                        <div className="space-y-5">
                            <div className="flex flex-col gap-3">
                                <div className={`h-2.5 rounded-full w-full bg-gradient-to-r ${selectedMetric.isDivergent ? 'from-[#006837] via-white to-[#bd0026]' : (selectedMetric.higherTheBetter ? 'from-[#f7fbff] to-[#08306b]' : 'from-[#fff5f0] to-[#67000d]')}`} />
                                <div className="flex justify-between text-[9px] font-black opacity-40 uppercase tracking-tighter">
                                    <span>{selectedMetric.format(currentDomain[0])}</span>
                                    <span>{selectedMetric.format(currentDomain[1])}</span>
                                </div>
                            </div>
                            <div className={`pt-4 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-200'}`}>
                                <p className="text-[11px] font-black leading-tight mb-2 uppercase tracking-tight">{selectedMetric.label} {selectedMetric.unit ? `(${selectedMetric.unit})` : ''}</p>
                                <p className="text-[9px] opacity-40 leading-relaxed font-bold uppercase tracking-tight">{selectedMetric.description || `Spatial distribution and variance of ${selectedMetric.label.toLowerCase()} across the ${viewLevel} network.`}</p>
                            </div>
                        </div>
                    </div>
                </div>

                <div className="flex-1">
                    <MapContainer center={[38.74, -9.14]} zoom={11} className="h-full w-full" zoomControl={false} style={{ background: isDarkMode ? '#0a0a0a' : '#f0f0f0' }}>
                        <ZoomHandler extent={nutFilter === REGION_KEYS[0] ? DEFAULT_REGION : nutFilter} />
                        <TileLayer url={isDarkMode ? "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png" : "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"} attribution='&copy; CARTO' />
                        {activeGeoData?.features && (
                            <GeoJSON key={`${viewLevel}-${nutFilter}-${selectedMetricId}-${isDarkMode}-${selectedFeature?.id}`} data={activeGeoData as any} style={getStyle} onEachFeature={onEachFeature} />
                        )}
                        {viewLevel !== 'municipality' && dataState.limits && (
                            <GeoJSON data={dataState.limits as any} style={{ fillOpacity: 0, weight: 4, color: isDarkMode ? 'rgba(255,255,255,0.4)' : 'rgba(0,0,0,0.3)' }} interactive={false} />
                        )}
                    </MapContainer>
                </div>
            </div>

            {/* Sidebar Right: Analytics */}
            <div className={`w-[420px] flex flex-col ${isDarkMode ? 'bg-neutral-900 border-neutral-800' : 'bg-white border-neutral-200'} border-l shadow-2xl z-30 overflow-hidden`}>
                <div className="flex-1 overflow-y-auto p-8 space-y-10 scrollbar-hide">

                    {/* Selection Detail */}
                    <section>
                        <h3 className="text-[10px] font-black opacity-30 uppercase tracking-[0.3em] mb-5 flex items-center gap-2">
                            <MapPin className="w-3.5 h-3.5 text-indigo-500" /> Regional Intelligence
                        </h3>
                        {selectedFeature ? (
                            <div className={`${isDarkMode ? 'bg-neutral-800/40 border-neutral-700/50' : 'bg-neutral-50 border-neutral-100'} rounded-[32px] p-7 border shadow-sm`}>
                                <div className="mb-6">
                                    <span className={`text-[9px] font-black ${isDarkMode ? 'text-indigo-400' : 'text-indigo-600'} uppercase tracking-[0.2em]`}>{selectedFeature.group_id || 'LMA'}</span>
                                    <h3 className="font-bold text-xl leading-tight mt-1.5 tracking-tight">{selectedFeature.name || selectedFeature.id}</h3>
                                </div>

                                <div className="grid grid-cols-2 gap-4 mb-6">
                                    <DetailCard label="Poverty Index" value={(selectedFeature.IMPT_score_pca_avg || 0).toFixed(1)} color="text-red-400" isDark={isDarkMode} />
                                    <DetailCard label="Residents" value={(selectedFeature.census_residents || 0).toLocaleString()} isDark={isDarkMode} />
                                </div>
                                {/* Modal Share Breakdown - Currently disabled as data format changed to be agnostic */}
                                {selectedFeature.share_car !== undefined && (
                                    <div className="mb-6 pt-6 border-t border-neutral-800/50">
                                        <h4 className="text-[10px] font-black opacity-30 uppercase mb-4 tracking-widest">Mobility Profile (Modal Share)</h4>
                                        <div className="h-16 flex items-center">
                                            <ResponsiveContainer width="100%" height="100%">
                                                <BarChart data={[{
                                                    name: 'Share',
                                                    car: (selectedFeature.share_car || 0) * 100,
                                                    pt: (selectedFeature.share_pt || 0) * 100,
                                                    walk: (selectedFeature.share_walk || 0) * 100,
                                                    bike: (selectedFeature.share_bike || 0) * 100
                                                }]} layout="vertical">
                                                    <XAxis type="number" hide domain={[0, 100]} />
                                                    <YAxis dataKey="name" type="category" hide />
                                                    <RechartsTooltip cursor={false} content={({ payload }) => {
                                                        if (payload && payload.length) {
                                                            return (
                                                                <div className={`${isDarkMode ? 'bg-neutral-800 border-neutral-700' : 'bg-white border-neutral-100'} p-3 rounded-xl border shadow-xl flex flex-col gap-1`}>
                                                                    {payload.map((p: any) => (
                                                                        <div key={p.name} className="flex justify-between gap-4 text-[10px] items-center">
                                                                            <div className="flex items-center gap-1.5">
                                                                                <div className="w-2 h-2 rounded-full" style={{ backgroundColor: p.color }} />
                                                                                <span className="font-bold opacity-60 uppercase">{p.name}</span>
                                                                            </div>
                                                                            <span className="font-black text-indigo-500">{p.value.toFixed(1)}%</span>
                                                                        </div>
                                                                    ))}
                                                                </div>
                                                            );
                                                        }
                                                        return null;
                                                    }} />
                                                    <Bar dataKey="car" stackId="a" fill="#ef4444" radius={[4, 0, 0, 4]}>
                                                        <LabelList dataKey="car" position="insideLeft" formatter={(v: number) => v > 15 ? `${v.toFixed(0)}%` : ''} style={{ fontSize: '9px', fill: 'white', fontWeight: 'bold' }} />
                                                    </Bar>
                                                    <Bar dataKey="pt" stackId="a" fill="#6366f1" />
                                                    <Bar dataKey="walk" stackId="a" fill="#10b981" />
                                                    <Bar dataKey="bike" stackId="a" fill="#eab308" radius={[0, 4, 4, 0]} />
                                                </BarChart>
                                            </ResponsiveContainer>
                                        </div>
                                        <div className="flex justify-between mt-2 px-1">
                                            <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-red-500" /><span className="text-[8px] font-black opacity-40 uppercase">Car</span></div>
                                            <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-indigo-500" /><span className="text-[8px] font-black opacity-40 uppercase">PT</span></div>
                                            <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-emerald-500" /><span className="text-[8px] font-black opacity-40 uppercase">Walk</span></div>
                                            <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-yellow-500" /><span className="text-[8px] font-black opacity-40 uppercase">Bike</span></div>
                                        </div>
                                    </div>
                                )}
                                {viewLevel === 'municipality' && subLevelData.length > 0 && (
                                    <div className="mt-8 pt-6 border-t border-neutral-800/50">
                                        <h4 className="text-[10px] font-black opacity-30 uppercase mb-4 tracking-widest">Constituent Dynamics</h4>
                                        <div className="space-y-3 max-h-40 overflow-y-auto pr-2 scrollbar-thin">
                                            {subLevelData.slice(0, 10).map(f => (
                                                <div key={f.id || f.name} className="flex justify-between items-center text-[10px] hover:bg-neutral-800/30 p-1.5 rounded-lg transition-colors cursor-default">
                                                    <span className="opacity-50 truncate w-36">{f.name}</span>
                                                    <span className="font-bold text-indigo-400">{selectedMetric.format(f[selectedMetric.id] || 0)}</span>
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                )}
                            </div>
                        ) : (
                            <div className={`p-14 border border-dashed rounded-[32px] flex flex-col items-center justify-center text-center ${isDarkMode ? 'border-neutral-800' : 'border-neutral-200'}`}>
                                <MousePointer2 className="w-10 h-10 opacity-5 mb-4" />
                                <p className="text-[10px] font-bold uppercase opacity-20 leading-loose tracking-widest">Target a regional unit<br />for deep analytics</p>
                            </div>
                        )}
                    </section>

                    {/* Comparative Analytics */}
                    <section>
                        <h3 className="text-[10px] font-black opacity-30 uppercase tracking-[0.3em] mb-5 flex items-center gap-2">
                            <TrendingUp className="w-3.5 h-3.5 text-indigo-500" /> Metropolitan Contrast
                        </h3>
                        <div className="space-y-8">
                            <div>
                                <p className="text-[10px] font-bold opacity-50 mb-3 px-1 uppercase tracking-tighter">Top performers</p>
                                <div className={`h-44 rounded-2xl p-4 border shadow-inner ${isDarkMode ? 'bg-neutral-800/20 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                                    <MiniBarChart data={chartData.top10} metric={selectedMetric} isDark={isDarkMode} type="highest" />
                                </div>
                            </div>
                            <div>
                                <p className="text-[10px] font-bold opacity-50 mb-3 px-1 uppercase tracking-tighter">Low performers</p>
                                <div className={`h-44 rounded-2xl p-4 border shadow-inner ${isDarkMode ? 'bg-neutral-800/20 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                                    <MiniBarChart data={chartData.worst10} metric={selectedMetric} isDark={isDarkMode} type="lowest" />
                                </div>
                            </div>
                        </div>
                    </section>
                </div>
            </div>

            {/* About Overlay */}
            {showAbout && (
                <div className="fixed inset-0 z-[5000] flex items-center justify-center bg-black/85 backdrop-blur-xl p-8" onClick={() => setShowAbout(false)}>
                    <div className={`${isDarkMode ? 'bg-neutral-900 border-neutral-800 shadow-[0_0_50px_rgba(0,0,0,1)]' : 'bg-white border-neutral-200 shadow-2xl'} border rounded-[48px] max-w-2xl w-full p-14 relative transition-all animate-in zoom-in-95 duration-300`} onClick={e => e.stopPropagation()}>
                        <button onClick={() => setShowAbout(false)} className="absolute top-10 right-10 p-3 hover:bg-neutral-200 rounded-full transition-colors flex items-center justify-center"><X className="w-6 h-6 opacity-40 text-neutral-500" /></button>
                        <div className="flex items-center gap-6 mb-12">
                            <div className="w-16 h-16 bg-indigo-600 rounded-[20px] flex items-center justify-center shadow-2xl shadow-indigo-500/20"><Activity className="text-white w-9 h-9" /></div>
                            <div>
                                <h2 className="text-3xl font-black leading-none tracking-tighter">Pannapp</h2>
                                <p className="text-indigo-500 font-black text-[10px] uppercase tracking-[0.4em] mt-3">Governance Strategic Intelligence</p>
                            </div>
                        </div>
                        <div className="space-y-8 text-sm font-bold uppercase tracking-widest leading-relaxed opacity-60">
                            <p className={`p-6 rounded-2xl ${isDarkMode ? 'bg-white/5' : 'bg-neutral-50'} italic border-l-4 border-indigo-600 font-medium`}>"Decentralized Spatial Intelligence for Sustainable & Equitable Urban Governance Transitions."</p>
                            <div className="grid grid-cols-2 gap-4 pt-4">
                                <a href="https://github.com/U-Shift/IMPT-data" target="_blank" className="flex items-center gap-4 p-5 rounded-2xl border border-neutral-800 hover:bg-indigo-600 hover:text-white transition-all hover:border-indigo-600 hover:scale-[1.02] active:scale-95">
                                    <Github className="w-5 h-5" /> <span>Source Code</span>
                                </a>
                                <a href="https://ushift.tecnico.ulisboa.pt/" target="_blank" className="flex items-center gap-4 p-5 rounded-2xl border border-neutral-800 hover:bg-indigo-600 hover:text-white transition-all hover:border-indigo-600 hover:scale-[1.02] active:scale-95">
                                    <ExternalLink className="w-5 h-5" /> <span>U-Shift Lab</span>
                                </a>
                            </div>
                            <div className="pt-6 flex justify-between items-center opacity-40">
                                <p className="text-[10px]">Project Framework: <strong>IMPT Project</strong></p>
                                <p className="text-[10px]">IST - University of Lisbon</p>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {/* Download Overlay */}
            {showDownload && (
                <div className="fixed inset-0 z-[5000] flex items-center justify-center bg-black/85 backdrop-blur-xl p-8" onClick={() => setShowDownload(false)}>
                    <div className={`${isDarkMode ? 'bg-neutral-900 border-neutral-800 shadow-[0_0_50px_rgba(0,0,0,1)]' : 'bg-white border-neutral-200 shadow-2xl'} border rounded-[48px] max-w-2xl w-full p-14 relative transition-all animate-in zoom-in-95 duration-300`} onClick={e => e.stopPropagation()}>
                        <button onClick={() => setShowDownload(false)} className="absolute top-10 right-10 p-3 hover:bg-neutral-200 rounded-full transition-colors flex items-center justify-center"><X className="w-6 h-6 opacity-40 text-neutral-500" /></button>
                        <div className="flex items-center gap-6 mb-12">
                            <div className="w-16 h-16 bg-emerald-600 rounded-[20px] flex items-center justify-center shadow-2xl shadow-emerald-500/20"><Download className="text-white w-9 h-9" /></div>
                            <div>
                                <h2 className="text-3xl font-black leading-none tracking-tighter">Data Center</h2>
                                <p className="text-emerald-500 font-black text-[10px] uppercase tracking-[0.4em] mt-3">Metropolitan Insights Repository</p>
                            </div>
                        </div>

                        <div className="space-y-6">
                            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                                <DownloadCard
                                    title="Freguesias"
                                    id="freg_2024"
                                    isDark={isDarkMode}
                                    data={dataState.geo['freguesia']}
                                    filename="impt_lisbon_freguesias"
                                />
                                <DownloadCard
                                    title="Municipality"
                                    id="mun_2024"
                                    isDark={isDarkMode}
                                    data={dataState.geo['municipality']}
                                    filename="impt_lisbon_municipalities"
                                />
                                <DownloadCard
                                    title="Grid (Hex)"
                                    id="grid_h3_r8"
                                    isDark={isDarkMode}
                                    data={dataState.geo['hex']}
                                    filename="impt_lisbon_grid"
                                />
                            </div>

                            <div className={`p-6 rounded-3xl ${isDarkMode ? 'bg-white/5 border-neutral-800' : 'bg-neutral-50 border-neutral-100'} border mt-6`}>
                                <h4 className="text-[10px] font-black opacity-40 uppercase tracking-widest mb-3">About the data</h4>
                                <p className="text-[11px] leading-relaxed opacity-60 font-medium">All datasets are provided in <span className="font-bold text-indigo-500">GeoJSON</span> for spatial analysis and <span className="font-bold text-emerald-500">CSV</span> for tabular processing. Coordinates use <span className="font-bold">WGS84 (EPSG:4326)</span>. The unique identifier (Dicom/HexID) should be used to relate the files.</p>
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
};

const DownloadCard = ({ title, id, isDark, data, filename }: { title: string, id: string, isDark: boolean, data: any, filename: string }) => {
    const downloadCSV = () => {
        if (!data || !data.features) return;
        const properties = data.features.map((f: any) => f.properties);
        if (properties.length === 0) return;

        const keys = Object.keys(properties[0]);
        const csvContent = [
            keys.join(','),
            ...properties.map((row: any) => keys.map(k => {
                const val = row[k];
                return typeof val === 'string' ? `"${val.replace(/"/g, '""')}"` : val;
            }).join(','))
        ].join('\n');

        const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
        const link = document.createElement("a");
        link.href = URL.createObjectURL(blob);
        link.setAttribute("download", `${filename}.csv`);
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    };

    const downloadGeoJSON = () => {
        if (!data) return;
        const blob = new Blob([JSON.stringify(data)], { type: 'application/json' });
        const link = document.createElement("a");
        link.href = URL.createObjectURL(blob);
        link.setAttribute("download", `${filename}.json`);
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    };

    return (
        <div className={`${isDark ? 'bg-neutral-800/40 border-neutral-700/50' : 'bg-white border-neutral-200 shadow-sm'} p-6 rounded-[32px] border flex flex-col items-center gap-4`}>
            <div className="text-center">
                <h3 className="text-sm font-black tracking-tight">{title}</h3>
                <p className="text-[9px] font-bold opacity-30 mt-1 uppercase tracking-tighter">ID: {id}</p>
            </div>
            <div className="flex flex-col gap-2 w-full">
                <button onClick={downloadGeoJSON} className="w-full py-2.5 rounded-xl bg-indigo-600 hover:bg-indigo-500 text-white text-[10px] font-black uppercase tracking-widest transition-all">GeoJSON</button>
                <button onClick={downloadCSV} className="w-full py-2.5 rounded-xl border border-emerald-600/30 text-emerald-500 hover:bg-emerald-600 hover:text-white text-[10px] font-black uppercase tracking-widest transition-all">CSV</button>
            </div>
        </div>
    );
};

const DetailCard = ({ label, value, color = "", isDark = true }: { label: string, value: string, color?: string, isDark?: boolean }) => (
    <div className={`${isDark ? 'bg-neutral-800/60 border-neutral-700/30' : 'bg-neutral-50 border-neutral-100 shadow-sm'} p-4 rounded-2xl border transition-all hover:border-indigo-500/30`}>
        <span className="block text-[9px] font-black opacity-30 uppercase mb-1.5 tracking-widest">{label}</span>
        <span className={`text-sm font-black tracking-tighter ${color || (isDark ? 'text-white' : 'text-neutral-900')}`}>{value || '—'}</span>
    </div>
);

const MiniBarChart = ({ data, metric, isDark, type }: { data: any[], metric: MetricDef, isDark: boolean, type: 'highest' | 'lowest' }) => {
    // Logic: 
    // If higherTheBetter is true: highest is good (blue), lowest is critical (red)
    // If higherTheBetter is false: highest is bad (red), lowest is good (blue)
    const isGood = (type === 'highest' && metric.higherTheBetter) || (type === 'lowest' && !metric.higherTheBetter);
    const chartColor = isGood ? '#6366f1' : '#ef4444';

    return (
        <ResponsiveContainer width="100%" height="100%">
            <BarChart data={data} layout="vertical" margin={{ left: -35, right: 35, top: 0, bottom: 0 }}>
                <XAxis type="number" hide />
                <YAxis dataKey="name" type="category" hide />
                <RechartsTooltip cursor={{ fill: 'rgba(99, 102, 241, 0.05)' }} content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                        const d = payload[0].payload;
                        return (
                            <div className={`${isDark ? 'bg-neutral-800 border-neutral-700' : 'bg-white border-neutral-100'} p-3 rounded-xl border shadow-xl flex flex-col gap-1 z-[100]`}>
                                <div className="text-[10px] font-black opacity-30 uppercase tracking-widest">{d.group || 'District'}</div>
                                <div className="text-[11px] font-black">{d.name}</div>
                                <div className="text-[11px] font-black text-indigo-500 mt-1">{metric.format(d.value)} {metric.unit || ''}</div>
                            </div>
                        );
                    }
                    return null;
                }} />
                <Bar dataKey="value" radius={[0, 4, 4, 0]} barSize={12}>
                    {data.map((entry, index) => (
                        <Cell key={`cell-${index}`} fill={chartColor} fillOpacity={1 - (index * 0.08)} />
                    ))}
                </Bar>
            </BarChart>
        </ResponsiveContainer>
    );
};

export default Dashboard;
