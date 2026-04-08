import React from 'react';
import { MapPin, TrendingUp } from 'lucide-react';
import { BarChart, Bar, XAxis, YAxis, Tooltip as RechartsTooltip, ResponsiveContainer, LabelList } from 'recharts';
import { MetricDef } from '../types';
import { DetailCard } from './DetailCard';
import { MiniBarChart } from './MiniBarChart';
import { LEVEL_CONFIG, FLAT_METRICS } from '../constants';
import { getColor, isMetricValueIgnored, getMetricValue } from '../utils';
import { useTranslation } from 'react-i18next';

interface SidebarRightProps {
    isDarkMode: boolean;
    selectedFeature: any;
    viewLevel: string;
    selectedMetric: MetricDef;
    selectedMetricId: string;
    selectedMode: any;
    dataState: any;
    allDomains: Record<string, number[]>;
    subLevelData: any[];
    chartData: { bestPerformers: any[], worstPerformers: any[] };
    setSelectedFeature: (feat: any) => void;
    computedGeoData: any;
    setZoomRequest: (req: any) => void;
    setViewLevel: (l: any) => void;
    selectedVariations: Record<string, string>;
}

export const SidebarRight: React.FC<SidebarRightProps> = ({
    isDarkMode, selectedFeature, viewLevel, selectedMetric, selectedMetricId,
    selectedMode, dataState, allDomains, subLevelData, chartData,
    setSelectedFeature, computedGeoData, setZoomRequest, setViewLevel,
    selectedVariations
}) => {
    const { t } = useTranslation();
    return (
        <div className={`absolute top-4 right-4 w-[400px] max-h-[calc(100vh-22rem)] flex flex-col ${isDarkMode ? 'bg-neutral-900/95 border-neutral-800' : 'bg-white/95 border-neutral-200'} border rounded-[32px] shadow-2xl z-[1001] backdrop-blur-xl transition-all overflow-hidden`}>
            <div className="flex-1 overflow-y-auto p-7 space-y-8 scrollbar-hide">

                {/* Selection Detail - Visible only when an area is selected */}
                {selectedFeature && (
                    <section className="animate-in fade-in slide-in-from-right-4 duration-300">
                        <div className="flex items-center justify-between mb-5">
                            <h3 className="text-[12px] font-black opacity-30 uppercase tracking-[0.3em] flex items-center gap-2">
                                <MapPin className="w-3.5 h-3.5 text-sky-800" /> {t('sidebar.area_details')}
                            </h3>
                            <button
                                onClick={() => setSelectedFeature(null)}
                                className={`text-[10px] font-black uppercase tracking-widest px-2 py-1 rounded-lg ${isDarkMode ? 'hover:bg-white/5 text-neutral-500' : 'hover:bg-neutral-100 text-neutral-400'}`}
                            >
                                {t('common.close')}
                            </button>
                        </div>
                        <div className={`${isDarkMode ? 'bg-neutral-800/40 border-neutral-700/50' : 'bg-neutral-50 border-neutral-100'} rounded-[32px] p-6 border shadow-sm`}>
                            <div className="mb-6">
                                <span className={`text-[13px] font-black ${isDarkMode ? 'text-sky-700' : 'text-sky-900'} uppercase tracking-[0.2em]`}>
                                    {(() => {
                                        const parentLevel = (LEVEL_CONFIG as any)[viewLevel].parent;
                                        return (parentLevel && selectedFeature.group_id)
                                            ? (dataState.parentLookup[`${parentLevel}-${selectedFeature.group_id}`] || selectedFeature.group_id)
                                            : '';
                                    })()}
                                </span>
                                <h3 className="font-bold text-xl leading-tight mt-1.5 tracking-tight">{selectedFeature.name || selectedFeature.id}</h3>
                            </div>

                            <div className="grid grid-cols-2 gap-4 mb-6">
                                {FLAT_METRICS_FILTERED(selectedMetricId, selectedMode, selectedFeature, allDomains, isDarkMode, t, selectedVariations, viewLevel)}
                            </div>

                            {/* Modal Share Breakdown */}
                            {selectedFeature.share_car !== undefined && (
                                <div className="mb-6 pt-6 border-t border-neutral-800/50">
                                    <h4 className="text-[12px] font-black opacity-30 uppercase mb-4 tracking-widest">{t('sidebar.mobility_profile')}</h4>
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
                                                                    <div key={p.name} className="flex justify-between gap-4 text-[12px] items-center">
                                                                        <div className="flex items-center gap-1.5">
                                                                            <div className="w-2 h-2 rounded-full" style={{ backgroundColor: p.color }} />
                                                                            <span className="font-bold opacity-60 uppercase">{t(`modes.${p.name.toLowerCase()}`)}</span>
                                                                        </div>
                                                                        <span className="font-black text-sky-800">{p.value.toFixed(1)}%</span>
                                                                    </div>
                                                                ))}
                                                            </div>
                                                        );
                                                    }
                                                    return null;
                                                }} />
                                                <Bar dataKey="car" stackId="a" fill="#ef4444" radius={[4, 0, 0, 4]}>
                                                    <LabelList dataKey="car" position="insideLeft" formatter={(v: any) => (typeof v === 'number' && v > 15) ? `${v.toFixed(0)}%` : ''} style={{ fontSize: '9px', fill: 'white', fontWeight: 'bold' }} />
                                                </Bar>
                                                <Bar dataKey="pt" stackId="a" fill="#075985" />
                                                <Bar dataKey="walk" stackId="a" fill="#10b981" />
                                                <Bar dataKey="bike" stackId="a" fill="#eab308" radius={[0, 4, 4, 0]} />
                                            </BarChart>
                                        </ResponsiveContainer>
                                    </div>
                                    <div className="flex justify-between mt-2 px-1">
                                        <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-red-500" /><span className="text-[12px] font-black opacity-40 uppercase">{t('modes.car')}</span></div>
                                        <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-sky-800" /><span className="text-[12px] font-black opacity-40 uppercase">{t('modes.pt')}</span></div>
                                        <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-emerald-500" /><span className="text-[12px] font-black opacity-40 uppercase">{t('modes.walk')}</span></div>
                                        <div className="flex flex-col items-center gap-1"><div className="w-1.5 h-1.5 rounded-full bg-yellow-500" /><span className="text-[12px] font-black opacity-40 uppercase">{t('modes.bike')}</span></div>
                                    </div>
                                </div>
                            )}
                            {viewLevel === 'municipality' && Object.values(LEVEL_CONFIG).some(config => config.parent === viewLevel) && subLevelData.length > 0 && (
                                <div className="mt-8 pt-6 border-t border-neutral-800/50">
                                    <h4 className="text-[12px] font-black opacity-30 uppercase mb-4 tracking-widest">{t('sidebar.constituent_dynamics')}</h4>
                                    <div className="space-y-3 max-h-40 overflow-y-auto pr-2 scrollbar-hide">
                                        {subLevelData.slice(0, 10).map((f: any) => {
                                            const val = getMetricValue(f, selectedMetric, selectedMode, viewLevel, selectedVariations);
                                            const isIgnored = isMetricValueIgnored(val, selectedMetric);
                                            return (
                                                <div
                                                    key={f.id || f.name}
                                                    onClick={() => {
                                                        const childLevel = (Object.keys(LEVEL_CONFIG) as any).find((l: any) => (LEVEL_CONFIG as any)[l].parent === viewLevel);
                                                        if (childLevel) {
                                                            setViewLevel(childLevel);
                                                            setSelectedFeature(f);
                                                            setZoomRequest({ id: f.id, timestamp: Date.now() });
                                                        }
                                                    }}
                                                    className="flex justify-between items-center text-[12px] hover:bg-neutral-800/30 p-1.5 rounded-lg transition-colors cursor-pointer"
                                                >
                                                    <span className="opacity-50 w-36">{f.name}</span>
                                                    <span className="font-bold text-sky-800">
                                                        {!isIgnored ?
                                                            selectedMetric.format(val, allDomains[selectedMetric.id]?.[0] || 0, allDomains[selectedMetric.id]?.[allDomains[selectedMetric.id].length - 1] || 1)
                                                            : '—'
                                                        }
                                                    </span>
                                                </div>
                                            );
                                        })}
                                    </div>
                                </div>
                            )}
                        </div>
                    </section>
                )}

                {/* Comparative Analytics - Visible only when NO area is selected */}
                {!selectedFeature && (
                    <section className="animate-in fade-in slide-in-from-right-4 duration-300">
                        <h3 className="text-[12px] font-black opacity-30 uppercase tracking-[0.3em] mb-5 flex items-center gap-2">
                            <TrendingUp className="w-3.5 h-3.5 text-sky-800" /> {t('sidebar.regional_contrast')}
                        </h3>
                        <div className="space-y-8">
                            <div>
                                <p className="text-[12px] font-bold opacity-50 mb-3 px-1 uppercase tracking-tighter">{t('sidebar.top_performers')}</p>
                                <div className={`h-44 rounded-[28px] p-5 border shadow-inner ${isDarkMode ? 'bg-neutral-800/20 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                                    <MiniBarChart
                                        data={chartData.bestPerformers}
                                        metric={selectedMetric}
                                        isDark={isDarkMode}
                                        type="highest"
                                        onSelect={(id) => {
                                            const f = computedGeoData.features.find((feat: any) => String(feat.properties.id) === String(id));
                                            if (f) {
                                                setSelectedFeature(f.properties);
                                                setZoomRequest({ id, timestamp: Date.now() });
                                            }
                                        }}
                                    />
                                </div>
                            </div>
                            <div>
                                <p className="text-[12px] font-bold opacity-50 mb-3 px-1 uppercase tracking-tighter">{t('sidebar.low_performers')}</p>
                                <div className={`h-44 rounded-[28px] p-5 border shadow-inner ${isDarkMode ? 'bg-neutral-800/20 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                                    <MiniBarChart
                                        data={chartData.worstPerformers}
                                        metric={selectedMetric}
                                        isDark={isDarkMode}
                                        type="lowest"
                                        onSelect={(id) => {
                                            const f = computedGeoData.features.find((feat: any) => String(feat.properties.id) === String(id));
                                            if (f) {
                                                setSelectedFeature(f.properties);
                                                setZoomRequest({ id, timestamp: Date.now() });
                                            }
                                        }}
                                    />
                                </div>
                            </div>
                        </div>
                    </section>
                )}
            </div>
        </div>
    );
};



const FLAT_METRICS_FILTERED = (selectedMetricId: string, selectedMode: any, selectedFeature: any, allDomains: any, isDarkMode: boolean, t: any) => {
    return FLAT_METRICS.filter(m =>
        m.showAlwaysOnDetails || m.id === selectedMetricId
    )
        .sort((a, b) => {
            if (a.id === selectedMetricId) return -1;
            if (b.id === selectedMetricId) return 1;
            return 0;
        }).map(m => {
            const effectiveId = `${m.id}${selectedMode.suffix}`;
            const fallbackId = selectedMode.suffixFallback !== undefined ? `${m.id}${selectedMode.suffixFallback}` : undefined;
            const val = (selectedFeature[effectiveId] ?? (fallbackId ? selectedFeature[fallbackId] : undefined));
            if (isMetricValueIgnored(val, m)) {
                return null;
            }
            const isSelected = m.id === selectedMetricId;
            return (
                <div key={m.id} className={isSelected ? "col-span-2" : ""}>
                    <DetailCard
                        key={m.id}
                        label={t(m.label)}
                        value={m.format(val, allDomains[m.id]?.[0] || 0, allDomains[m.id]?.[allDomains[m.id].length - 1] || 1)}
                        hexColor={getColor(val, allDomains[m.id] || [0, 1], m)}
                        isDark={isDarkMode}
                    />
                </div>
            );
        });
};
