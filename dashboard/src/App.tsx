
import { useState, useMemo, useEffect } from 'react';
import { MapContainer, TileLayer, GeoJSON, useMap } from 'react-leaflet';
import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import { BarChart, Bar, XAxis, YAxis, Tooltip as RechartsTooltip, ResponsiveContainer, Cell, LabelList, PieChart, Pie, Legend as RechartsLegend } from 'recharts';
import { Info, MapPin, ChevronRight, X, Github, Activity, Loader2, MousePointer2, ChevronDown, Sun, Moon, ExternalLink, ListFilter, TrendingUp, AlertTriangle } from 'lucide-react';

// --- Types & Constants ---

type MetricDef = {
    id: string;
    label: string;
    category: 'Mobility' | 'Accessibility' | 'Land Use' | 'Sociodemographic' | 'Standalone';
    icon: string;
    description?: string;
    format: (v: number) => string;
    isDivergent?: boolean;
    domain?: [number, number];
    invertColor?: boolean;
    unit?: string;
    viewLevel?: 'municipality' | 'all'; // Restrict visibility
    isFake?: boolean; // Label as placeholder data
};

const METRICS: MetricDef[] = [
    { id: 'mobility_poverty_index', label: 'Mobility Poverty Index', category: 'Standalone', icon: '📈', description: 'Composite index (0-100) based on accessibility, car share, and vehicle ownership.', format: (v) => (v || 0).toFixed(1), domain: [0, 80], unit: '' },

    // Mobility
    { id: 'share_pt', label: 'PT Modal Share', category: 'Mobility', icon: '🚌', format: (v) => `${((v || 0) * 100).toFixed(0)}%`, domain: [0, 0.6], unit: '%' },
    { id: 'share_car', label: 'Car Modal Share', category: 'Mobility', icon: '🚗', format: (v) => `${((v || 0) * 100).toFixed(0)}%`, domain: [0, 0.8], invertColor: true, unit: '%' },
    { id: 'share_soft', label: 'Active Modal Share', category: 'Mobility', icon: '🚶', format: (v) => `${((v || 0) * 100).toFixed(0)}%`, domain: [0, 0.5], unit: '%' },
    { id: 'total_motor_vehicles_per_hh', label: 'Motorized Vehicles /HH', category: 'Mobility', icon: '🛵', format: (v) => (v || 0).toFixed(2), domain: [0, 2], unit: 'veh/HH' },
    { id: 'avg_bicycles', label: 'Bicycles /HH', category: 'Mobility', icon: '🚲', format: (v) => (v || 0).toFixed(2), domain: [0, 1.5], unit: 'bike/HH' },
    { id: 'pct_hh_no_vehicle', label: '% Households no Vehicle', category: 'Mobility', icon: '🛑', format: (v) => `${(v || 0).toFixed(0)}%`, domain: [0, 50], invertColor: true, unit: '%' },
    { id: 'n_transit_stops', label: 'Transit Stops', category: 'Mobility', icon: '🚏', format: (v) => (v || 0).toFixed(0), domain: [0, 50], unit: 'stops', isFake: true },
    { id: 'peak_services', label: 'Peak Services', category: 'Mobility', icon: '⚡', format: (v) => (v || 0).toFixed(0), domain: [0, 100], unit: 'serv/h', isFake: true },
    { id: 'offpeak_services', label: 'Off-Peak Services', category: 'Mobility', icon: '⛅', format: (v) => (v || 0).toFixed(0), domain: [0, 50], unit: 'serv/h', isFake: true },
    { id: 'night_services', label: 'Night Services', category: 'Mobility', icon: '🌙', format: (v) => (v || 0).toFixed(0), domain: [0, 20], unit: 'serv/h', isFake: true },

    // Accessibility
    { id: 'accessibility_gap', label: 'Access Gap (PT vs Car)', category: 'Accessibility', icon: '⚖️', description: 'Difference in minutes between PT and Car travel times to healthcare.', format: (v) => `${(v || 0).toFixed(0)}m`, isDivergent: true, domain: [-20, 60], unit: 'min' },
    { id: 'time_pt_peak', label: 'Time to Healthcare (PT)', category: 'Accessibility', icon: '⏱️', format: (v) => `${(v || 0).toFixed(0)}m`, domain: [10, 60], invertColor: true, unit: 'min' },
    { id: 'time_pt_off', label: 'Time to Healthcare (Off-Peak)', category: 'Accessibility', icon: '🌥️', format: (v) => `${(v || 0).toFixed(0)}m`, domain: [10, 60], invertColor: true, unit: 'min', isFake: true },
    { id: 'time_pt_night', label: 'Time to Healthcare (Night)', category: 'Accessibility', icon: '🌑', format: (v) => `${(v || 0).toFixed(0)}m`, domain: [10, 80], invertColor: true, unit: 'min', isFake: true },
    { id: 'time_car', label: 'Time to Healthcare (Car)', category: 'Accessibility', icon: '🏎️', format: (v) => `${(v || 0).toFixed(0)}m`, domain: [5, 30], invertColor: true, unit: 'min' },

    // Land Use
    { id: 'infra_pedestrian', label: 'Pedestrian Infra Ratio', category: 'Land Use', icon: '👟', format: (v) => (v || 0).toFixed(2), domain: [0, 1], unit: '' },
    { id: 'infra_cycling', label: 'Cycling Infra Ratio', category: 'Land Use', icon: '🚴', format: (v) => (v || 0).toFixed(2), domain: [0, 1], unit: '' },
    { id: 'pct_pre_1945', label: 'Houses Pre-1945', category: 'Land Use', icon: '🏚️', format: (v) => `${((v || 0) * 100).toFixed(1)}%`, domain: [0, 0.5], unit: '%' },
    { id: 'poi_amenity', label: 'Civic Amenities', category: 'Land Use', icon: '🏢', format: (v) => (v || 0).toFixed(0), domain: [0, 100], unit: 'cnt' },
    { id: 'poi_healthcare', label: 'Health Facilities', category: 'Land Use', icon: '🏥', format: (v) => (v || 0).toFixed(0), domain: [0, 20], unit: 'cnt' },
    { id: 'poi_leisure', label: 'Leisure Spots', category: 'Land Use', icon: '⛲', format: (v) => (v || 0).toFixed(0), domain: [0, 50], unit: 'cnt' },
    { id: 'poi_shop', label: 'Local Shops', category: 'Land Use', icon: '🛍️', format: (v) => (v || 0).toFixed(0), domain: [0, 150], unit: 'cnt' },
    { id: 'poi_tourism', label: 'Tourism POIs', category: 'Land Use', icon: '📸', format: (v) => (v || 0).toFixed(0), domain: [0, 30], unit: 'cnt' },

    // Sociodemographic
    { id: 'income', label: 'Avg Income /Person', category: 'Sociodemographic', icon: '💰', format: (v) => `€${(v || 0).toLocaleString()}`, domain: [5000, 25000], unit: '€/y', viewLevel: 'municipality' },
    { id: 'gini', label: 'Gini Coefficient', category: 'Sociodemographic', icon: '⚖️', format: (v) => `${(v || 0).toFixed(1)}%`, domain: [30, 50], unit: '', viewLevel: 'municipality' },
    { id: 'pop_total', label: 'Total Population', category: 'Sociodemographic', icon: '👥', format: (v) => (v || 0).toLocaleString(), domain: [0, 50000], unit: 'inh' },
    { id: 'pop_density', label: 'Population Density', category: 'Sociodemographic', icon: '🏙️', format: (v) => `${(v || 0).toFixed(0)} /km²`, domain: [0, 15000], unit: 'inh/km²' },
    { id: 'pct_youth', label: 'Youth Ratio (<15)', category: 'Sociodemographic', icon: '👶', format: (v) => `${((v || 0) * 100).toFixed(1)}%`, domain: [0, 0.25], unit: '%' },
    { id: 'pct_elderly', label: 'Elderly Ratio (>65)', category: 'Sociodemographic', icon: '👵', format: (v) => `${((v || 0) * 100).toFixed(1)}%`, domain: [0, 0.4], unit: '%' },
    { id: 'pct_women', label: '% Women', category: 'Sociodemographic', icon: '👩', format: (v) => `${((v || 0) * 100).toFixed(1)}%`, domain: [0.4, 0.6], unit: '%' },
];


const COLORS = {
    Sequential: ['#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b'],
    Danger: ['#fff5f0', '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d'],
    Divergent: {
        negative: ['#006837', '#31a354', '#78c679', '#c2e699', '#f7fcb9'],
        positive: ['#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#f03b20', '#bd0026']
    }
};

const ZOOM_EXTENTS = {
    'all': { center: [38.74, -9.14], zoom: 11 },
    'Grande Lisboa': { center: [38.85, -9.15], zoom: 11 },
    'Península de Setúbal': { center: [38.55, -9.05], zoom: 11 }
};

const ZoomHandler = ({ extent }: { extent: 'all' | 'Grande Lisboa' | 'Península de Setúbal' }) => {
    const map = useMap();
    useEffect(() => {
        const config = ZOOM_EXTENTS[extent];
        map.setView(config.center as L.LatLngExpression, config.zoom);
    }, [extent, map]);
    return null;
};

const Dashboard = () => {
    const [viewLevel, setViewLevel] = useState<'hex' | 'freguesia' | 'municipality'>('freguesia');
    const [nutFilter, setNutFilter] = useState<'all' | 'Grande Lisboa' | 'Península de Setúbal'>('all');
    const [selectedMetricId, setSelectedMetricId] = useState<string>('mobility_poverty_index');
    const [selectedFeature, setSelectedFeature] = useState<any>(null);
    const [showAbout, setShowAbout] = useState(false);
    const [isDarkMode, setIsDarkMode] = useState(true);
    const [collapsedSections, setCollapsedSections] = useState<Record<string, boolean>>({
        'Mobility': true, 'Accessibility': true, 'Land Use': true, 'Sociodemographic': true
    });

    const [dataState, setDataState] = useState<{
        freguesias: any; municipios: any; hex: any; limits: any; loading: boolean; error: string | null;
    }>({ freguesias: null, municipios: null, hex: null, limits: null, loading: true, error: null });

    useEffect(() => {
        const load = async () => {
            try {
                const [f, m, h, l] = await Promise.all([
                    fetch('/data/freguesias_data.json').then(r => r.json()),
                    fetch('/data/municipios_data.json').then(r => r.json()),
                    fetch('/data/hex_data.json').then(r => r.json()),
                    fetch('/data/municipios_limits.json').then(r => r.json())
                ]);
                setDataState({ freguesias: f, municipios: m, hex: h, limits: l, loading: false, error: null });
            } catch (err) {
                console.error(err);
                setDataState(s => ({ ...s, loading: false, error: "System encountered a data loading error." }));
            }
        };
        load();
    }, []);

    const selectedMetric = useMemo(() => METRICS.find(m => m.id === selectedMetricId) || METRICS[0], [selectedMetricId]);

    const activeGeoData = useMemo(() => {
        if (!dataState.freguesias) return null;
        let raw = viewLevel === 'hex' ? dataState.hex : (viewLevel === 'municipality' ? dataState.municipios : dataState.freguesias);
        if (!raw || !raw.features) return { type: "FeatureCollection", features: [] };

        let features = raw.features;
        if (nutFilter !== 'all') {
            features = features.filter((f: any) => f.properties?.nuts2 === nutFilter);
        }
        return { ...raw, features };
    }, [viewLevel, nutFilter, dataState]);

    const currentDomain = useMemo(() => {
        const defaultDomain: [number, number] = selectedMetric.domain || [0, 1];
        if (!activeGeoData?.features?.length) return defaultDomain;
        const values = activeGeoData.features.map((f: any) => f.properties?.[selectedMetric.id]).filter((v: any) => v !== undefined && !isNaN(v));
        if (values.length === 0) return defaultDomain;
        return [Math.min(...values), Math.max(...values)] as [number, number];
    }, [activeGeoData, selectedMetric]);

    const getScaleColor = (value: number, metric: MetricDef, domain: [number, number]) => {
        if (metric.isDivergent) {
            if (value === 0) return '#ffffff';
            if (value < 0) {
                const normalized = Math.min(1, Math.abs(value) / (Math.abs(domain[0]) || 20));
                return COLORS.Divergent.negative[Math.floor(normalized * (COLORS.Divergent.negative.length - 1))];
            } else {
                const normalized = Math.min(1, value / (domain[1] || 60));
                return COLORS.Divergent.positive[Math.floor(normalized * (COLORS.Divergent.positive.length - 1))];
            }
        }
        const [min, max] = domain;
        const normalized = max === min ? 0.5 : Math.min(1, Math.max(0, (value - min) / (max - min)));
        const scale = (metric.invertColor || metric.id.includes('poverty') || metric.id.includes('gap')) ? COLORS.Danger : COLORS.Sequential;
        return scale[Math.floor(normalized * (scale.length - 1))];
    };

    const getStyle = (feature: any) => {
        const isSelected = selectedFeature && (
            (viewLevel === 'hex' && feature.properties.hex_id === selectedFeature.hex_id) ||
            (viewLevel === 'freguesia' && feature.properties.dtmnfr === selectedFeature.dtmnfr) ||
            (viewLevel === 'municipality' && feature.properties.municipio === selectedFeature.municipio)
        );
        return {
            fillColor: getScaleColor(feature.properties[selectedMetric.id] || 0, selectedMetric, currentDomain),
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
                <div style="font-size: 10px; font-weight: 900; color: #666; text-transform: uppercase;">${props.municipio || 'LMA'}</div>
                <div style="font-size: 12px; font-weight: 700; color: #111;">${props.freguesia || props.municipio || 'N/A'}</div>
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
        setCollapsedSections(prev => {
            const newState = { 'Mobility': true, 'Accessibility': true, 'Land Use': true, 'Sociodemographic': true };
            if (prev[cat]) newState[cat as keyof typeof newState] = false;
            return newState as any;
        });
    };

    const municipalityFreguesias = useMemo(() => {
        if (viewLevel !== 'municipality' || !selectedFeature || !dataState.freguesias) return [];
        return dataState.freguesias.features
            .filter((f: any) => f.properties?.municipio === selectedFeature.municipio)
            .map((f: any) => f.properties)
            .sort((a: any, b: any) => (b[selectedMetric.id] || 0) - (a[selectedMetric.id] || 0));
    }, [viewLevel, selectedFeature, selectedMetric, dataState]);

    const chartData = useMemo(() => {
        if (!activeGeoData?.features) return { top10: [], worst10: [] };
        const feats = activeGeoData.features.map((f: any) => ({
            name: f.properties?.freguesia || f.properties?.municipio || f.properties?.id || 'Unknown',
            municipio: f.properties?.municipio || '',
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
                    <div className="flex items-center gap-3 mb-6">
                        <div className="w-10 h-10 bg-indigo-600 rounded-xl flex items-center justify-center shadow-lg">
                            <Activity className="text-white w-6 h-6" />
                        </div>
                        <div>
                            <h1 className="text-sm font-black tracking-tighter uppercase leading-none">Mobility <span className="text-indigo-500">Poverty</span></h1>
                            <p className="text-[10px] font-bold uppercase tracking-widest opacity-40 mt-1">Lisbon Metropolis</p>
                        </div>
                    </div>

                    <div className="flex items-center justify-between mb-4">
                        <div className="flex gap-1">
                            <button onClick={() => setIsDarkMode(!isDarkMode)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                {isDarkMode ? <Sun className="w-5 h-5" /> : <Moon className="w-5 h-5" />}
                            </button>
                            <button onClick={() => setShowAbout(true)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
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
                            <button
                                onClick={() => setSelectedMetricId('mobility_poverty_index')}
                                className={`w-full flex items-center justify-between px-4 py-3.5 rounded-2xl text-[11px] font-black transition-all ${selectedMetricId === 'mobility_poverty_index'
                                    ? 'bg-indigo-600 text-white shadow-xl scale-[1.02]'
                                    : (isDarkMode ? 'bg-neutral-800/50 hover:bg-neutral-800 text-neutral-300' : 'bg-neutral-100 hover:bg-neutral-200 text-neutral-600')}`}
                            >
                                <span className="flex items-center gap-3">📈 <span>Mobility Poverty Index</span></span>
                                <ChevronRight className={`w-4 h-4 ${selectedMetricId === 'mobility_poverty_index' ? 'opacity-100' : 'opacity-0'}`} />
                            </button>

                            {(['Mobility', 'Accessibility', 'Land Use', 'Sociodemographic'] as const).map(cat => (
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
                                            {METRICS.filter(m => m.category === cat).map(m => {
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
                    <div className="flex gap-4 pointer-events-auto">
                        <div className={`${isDarkMode ? 'bg-neutral-900/90 border-neutral-800 shadow-2xl' : 'bg-white/90 border-neutral-200 shadow-xl'} backdrop-blur-md px-1.5 py-1.5 rounded-2xl border flex items-center`}>
                            {(['hex', 'freguesia', 'municipality'] as const).map(l => (
                                <button key={l} onClick={() => setViewLevel(l)}
                                    className={`px-5 py-2 rounded-xl text-[10px] font-black uppercase tracking-widest transition-all ${viewLevel === l ? 'bg-indigo-600 text-white shadow-xl' : `${isDarkMode ? 'text-neutral-500 hover:text-neutral-300' : 'text-neutral-400 hover:text-neutral-800'}`}`}
                                >{l === 'hex' ? 'Grid' : l}</button>
                            ))}
                        </div>
                        <div className={`${isDarkMode ? 'bg-neutral-900/90 border-neutral-800 shadow-2xl' : 'bg-white/90 border-neutral-200 shadow-xl'} backdrop-blur-md px-1.5 py-1.5 rounded-2xl border flex items-center`}>
                            {(['all', 'Grande Lisboa', 'Península de Setúbal'] as const).map(n => (
                                <button key={n} onClick={() => setNutFilter(n)}
                                    className={`px-5 py-2 rounded-xl text-[10px] font-black uppercase tracking-widest transition-all ${nutFilter === n ? 'bg-indigo-600 text-white shadow-xl' : `${isDarkMode ? 'text-neutral-500 hover:text-neutral-300' : 'text-neutral-400 hover:text-neutral-800'}`}`}
                                >{n === 'all' ? 'Metropolis' : n}</button>
                            ))}
                        </div>
                    </div>
                </div>

                <div className="absolute bottom-8 left-8 z-[1000] pointer-events-none w-[280px]">
                    <div className={`p-6 rounded-[32px] border pointer-events-auto shadow-2xl backdrop-blur-xl ${isDarkMode ? 'bg-neutral-900/90 border-neutral-800' : 'bg-white/90 border-neutral-100'}`}>
                        <h4 className="flex items-center gap-2.5 text-[10px] font-black text-indigo-500 mb-5 uppercase tracking-[0.1em]">
                            <Activity className="w-3.5 h-3.5" /> {nutFilter !== 'all' ? 'Local Rescaling' : 'Global Metric Scale'}
                        </h4>
                        <div className="space-y-5">
                            <div className="flex flex-col gap-3">
                                <div className={`h-2.5 rounded-full w-full bg-gradient-to-r ${selectedMetric.isDivergent ? 'from-[#006837] via-white to-[#bd0026]' : (selectedMetric.invertColor || selectedMetric.id.includes('poverty') ? 'from-[#fff5f0] to-[#67000d]' : 'from-[#f7fbff] to-[#08306b]')}`} />
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
                        <ZoomHandler extent={nutFilter} />
                        <TileLayer url={isDarkMode ? "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png" : "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"} attribution='&copy; CARTO' />
                        {activeGeoData?.features && (
                            <GeoJSON key={`${viewLevel}-${nutFilter}-${selectedMetricId}-${isDarkMode}-${selectedFeature?.dtmnfr || selectedFeature?.municipio}`} data={activeGeoData as any} style={getStyle} onEachFeature={onEachFeature} />
                        )}
                        {viewLevel !== 'municipality' && dataState.limits && (
                            <GeoJSON data={dataState.limits as any} style={{ fillOpacity: 0, weight: 2.5, color: isDarkMode ? 'rgba(255,255,255,0.2)' : 'rgba(0,0,0,0.1)' }} interactive={false} />
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
                                    <span className={`text-[9px] font-black ${isDarkMode ? 'text-indigo-400' : 'text-indigo-600'} uppercase tracking-[0.2em]`}>{selectedFeature.municipio}</span>
                                    <h3 className="font-bold text-xl leading-tight mt-1.5 tracking-tight">{selectedFeature.freguesia || selectedFeature.municipio}</h3>
                                </div>

                                <div className="grid grid-cols-2 gap-4 mb-6">
                                    <DetailCard label="Poverty Index" value={(selectedFeature.mobility_poverty_index || 0).toFixed(1)} color="text-red-400" isDark={isDarkMode} />
                                    <DetailCard label="Density" value={`${(selectedFeature.pop_density || 0).toFixed(0)}/k²`} isDark={isDarkMode} />
                                    <DetailCard label="Avg Income" value={selectedFeature.income ? `€${selectedFeature.income.toLocaleString()}` : 'Mun Level'} isDark={isDarkMode} />
                                    <DetailCard label="Gini" value={selectedFeature.gini ? `${selectedFeature.gini.toFixed(1)}%` : 'Mun Level'} isDark={isDarkMode} />
                                </div>

                                {/* Modal Share Breakdown */}
                                <div className="mb-6 pt-6 border-t border-neutral-800/50">
                                    <h4 className="text-[10px] font-black opacity-30 uppercase mb-4 tracking-widest">Mobility Profile</h4>
                                    <div className="h-32 flex items-center">
                                        <ResponsiveContainer width="100%" height="100%">
                                            <BarChart data={[
                                                { name: 'Car', val: (selectedFeature.share_car || 0) * 100 },
                                                { name: 'PT', val: (selectedFeature.share_pt || 0) * 100 },
                                                { name: 'Soft', val: (selectedFeature.share_soft || 0) * 100 }
                                            ]} layout="vertical" margin={{ left: -30 }}>
                                                <XAxis type="number" hide domain={[0, 100]} />
                                                <YAxis dataKey="name" type="category" style={{ fontSize: '9px', fontWeight: 'bold', fill: isDarkMode ? '#666' : '#999' }} />
                                                <Bar dataKey="val" radius={[0, 4, 4, 0]} barSize={12}>
                                                    {[0, 1, 2].map((i) => <Cell key={i} fill={['#ef4444', '#6366f1', '#10b981'][i]} />)}
                                                    <LabelList dataKey="val" position="right" formatter={(v: number) => `${v.toFixed(0)}%`} style={{ fontSize: '9px', fill: '#888', fontWeight: 'bold' }} />
                                                </Bar>
                                            </BarChart>
                                        </ResponsiveContainer>
                                    </div>
                                </div>

                                {/* Accessibility Scenarios */}
                                <div className="pt-6 border-t border-neutral-800/50">
                                    <h4 className="text-[10px] font-black opacity-30 uppercase mb-4 tracking-widest">Access Timeline (PT)</h4>
                                    <div className="space-y-3">
                                        <div className="flex justify-between items-center text-[10px]">
                                            <span className="opacity-50">Peak (08:00)</span>
                                            <span className="font-bold text-indigo-400">{(selectedFeature.time_pt_peak || 0).toFixed(0)}m</span>
                                        </div>
                                        <div className="flex justify-between items-center text-[10px]">
                                            <span className="opacity-50">Evening (20:00)</span>
                                            <span className="font-bold text-amber-400">{(selectedFeature.time_pt_off || 0).toFixed(0)}m</span>
                                        </div>
                                        <div className="flex justify-between items-center text-[10px]">
                                            <span className="opacity-50">Night (03:00)</span>
                                            <span className="font-bold text-red-400">{(selectedFeature.time_pt_night || 0).toFixed(0)}m</span>
                                        </div>
                                    </div>
                                </div>

                                {viewLevel === 'municipality' && municipalityFreguesias.length > 0 && (
                                    <div className="mt-8 pt-6 border-t border-neutral-800/50">
                                        <h4 className="text-[10px] font-black opacity-30 uppercase mb-4 tracking-widest">Constituent Dynamics</h4>
                                        <div className="space-y-3 max-h-40 overflow-y-auto pr-2 scrollbar-thin">
                                            {municipalityFreguesias.slice(0, 10).map(f => (
                                                <div key={f.dtmnfr || f.freguesia} className="flex justify-between items-center text-[10px] hover:bg-neutral-800/30 p-1.5 rounded-lg transition-colors cursor-default">
                                                    <span className="opacity-50 truncate w-36">{f.freguesia}</span>
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
                                <p className="text-[10px] font-bold opacity-50 mb-3 px-1 uppercase tracking-tighter">Critical Concentration</p>
                                <div className={`h-44 rounded-2xl p-4 border shadow-inner ${isDarkMode ? 'bg-neutral-800/20 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                                    <MiniBarChart data={chartData.top10} metric={selectedMetric} isDark={isDarkMode} type="highest" />
                                </div>
                            </div>
                            <div>
                                <p className="text-[10px] font-bold opacity-50 mb-3 px-1 uppercase tracking-tighter">Positive Concentration</p>
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
        </div>
    );
};

const DetailCard = ({ label, value, color = "", isDark = true }: { label: string, value: string, color?: string, isDark?: boolean }) => (
    <div className={`${isDark ? 'bg-neutral-800/60 border-neutral-700/30' : 'bg-neutral-50 border-neutral-100 shadow-sm'} p-4 rounded-2xl border transition-all hover:border-indigo-500/30`}>
        <span className="block text-[9px] font-black opacity-30 uppercase mb-1.5 tracking-widest">{label}</span>
        <span className={`text-sm font-black tracking-tighter ${color || (isDark ? 'text-white' : 'text-neutral-900')}`}>{value || '—'}</span>
    </div>
);

const MiniBarChart = ({ data, metric, isDark, type }: { data: any[], metric: MetricDef, isDark: boolean, type: 'highest' | 'lowest' }) => (
    <ResponsiveContainer width="100%" height="100%">
        <BarChart data={data} layout="vertical" margin={{ left: -35, right: 35, top: 0, bottom: 0 }}>
            <XAxis type="number" hide />
            <YAxis dataKey="name" type="category" hide />
            <RechartsTooltip cursor={{ fill: 'rgba(99, 102, 241, 0.05)' }} content={({ active, payload }) => {
                if (active && payload && payload.length) {
                    const d = payload[0].payload;
                    return (
                        <div className={`${isDark ? 'bg-neutral-900 border-neutral-800' : 'bg-white border-neutral-200 shadow-2xl'} border p-4 rounded-2xl backdrop-blur-2xl`}>
                            <p className="text-[9px] font-black opacity-40 uppercase mb-1.5 tracking-widest">{d.municipio || 'LISBON AREA'}</p>
                            <p className={`text-[11px] font-black ${isDark ? 'text-white' : 'text-neutral-900'} leading-tight mb-2 tracking-tight`}>{d.name}</p>
                            <p className={`text-sm font-black ${type === 'highest' ? 'text-red-400' : 'text-emerald-400'}`}>{metric.format(d.value)}</p>
                        </div>
                    );
                }
                return null;
            }} />
            <Bar dataKey="value" radius={[0, 6, 6, 0]} barSize={10}>
                {data.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={type === 'highest' ? '#ef4444' : '#10b981'} opacity={1 - index * 0.06} />
                ))}
                <LabelList dataKey="name" position="right" style={{ fontSize: '7px', fill: '#666', fontWeight: '900', textTransform: 'uppercase', letterSpacing: '0.05em' }} formatter={(v: string) => v.length > 18 ? v.substring(0, 16) + '..' : v} offset={10} />
            </Bar>
        </BarChart>
    </ResponsiveContainer>
);

export default Dashboard;
