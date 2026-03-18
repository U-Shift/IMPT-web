import React from 'react';
import { Sun, Moon, Download, Info, ListFilter, ChevronDown, AlertTriangle, Activity } from 'lucide-react';
import { MetricDef } from '../types';
import { METRICS, FLAT_METRICS } from '../constants';

interface SidebarLeftProps {
    isDarkMode: boolean;
    setIsDarkMode: (val: boolean) => void;
    setShowDownload: (val: boolean) => void;
    setShowAbout: (val: boolean) => void;
    selectedMetric: MetricDef;
    selectedMetricId: string;
    setSelectedMetricId: (val: string) => void;
    viewLevel: string;
    collapsedSections: Record<string, boolean>;
    toggleSection: (cat: string) => void;
    weights: Record<string, number>;
    setWeights: (val: any) => void;
    resetWeights: () => void;
    setIsAHPModalOpen: (val: boolean) => void;
}

export const SidebarLeft: React.FC<SidebarLeftProps> = ({
    isDarkMode, setIsDarkMode, setShowDownload, setShowAbout,
    selectedMetric, selectedMetricId, setSelectedMetricId,
    viewLevel, collapsedSections, toggleSection,
    weights, setWeights, resetWeights, setIsAHPModalOpen
}) => {
    return (
        <div className={`absolute top-4 left-4 w-[380px] max-h-[calc(100vh-2rem)] flex flex-col ${isDarkMode ? 'bg-neutral-900/95 border-neutral-800 shadow-2xl' : 'bg-white/95 border-neutral-200 shadow-2xl'} border rounded-[32px] backdrop-blur-xl z-[1001] transition-all overflow-hidden`}>
            <div className={`p-6 border-b ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>
                <div className="flex items-center gap-4 mb-6">
                    <img src="images/logo/icon.png" alt="Logo" className="w-12 h-12 object-contain" />
                    <div className="flex flex-col">
                        <h1 className="text-base font-black tracking-tighter uppercase leading-none">Mobility <span className="text-sky-800">Poverty</span> Index</h1>
                        <p className="text-[12px] font-bold uppercase tracking-widest opacity-40 mt-1">Lisbon Metro Area</p>
                    </div>
                </div>

                <div className="flex items-center justify-between mb-4">
                    <div className="flex gap-1">
                        <button onClick={() => setIsDarkMode(!isDarkMode)} data-tooltip="Toggle Theme" className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                            {isDarkMode ? <Sun className="w-5 h-5" /> : <Moon className="w-5 h-5" />}
                        </button>
                        <button onClick={() => setShowDownload(true)} data-tooltip="Download Data" className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                            <Download className="w-5 h-5" />
                        </button>
                        <button onClick={() => setShowAbout(true)} data-tooltip="About" className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                            <Info className="w-5 h-5" />
                        </button>
                    </div>
                    <div className={`px-2.5 py-1 rounded-lg text-[13px] font-black uppercase tracking-widest ${isDarkMode ? 'bg-neutral-800 text-neutral-500' : 'bg-neutral-100 text-neutral-400'}`}>Beta version</div>
                </div>
            </div>

            <div className="flex-1 overflow-y-auto p-6 space-y-8 scrollbar-hide">
                <section>
                    <h3 className="text-[12px] font-black opacity-30 uppercase tracking-[0.3em] mb-4 flex items-center gap-2">
                        <ListFilter className="w-3 h-3 text-sky-800" /> Indicators
                    </h3>
                    <div className="space-y-4">
                        {Object.keys(METRICS).map(cat => (
                            <div key={cat} className={`border rounded-2xl transition-all ${isDarkMode ? 'border-neutral-800 bg-neutral-800/10' : 'border-neutral-100 bg-neutral-50/30'} ${!collapsedSections[cat] ? 'ring-1 ring-sky-800/20' : ''}`}>
                                <button onClick={() => toggleSection(cat)}
                                    className={`w-full flex items-center justify-between px-4 py-3.5 text-[12px] font-black uppercase tracking-widest transition-colors ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-600'}`}
                                >
                                    <span className="flex items-center gap-2">
                                        <div className={`w-1.5 h-1.5 rounded-full ${selectedMetric.category === cat ? 'bg-sky-800 shadow-[0_0_8px_rgba(99,102,241,0.5)]' : 'bg-neutral-700'}`} />{cat}
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
                                                    data-tooltip={m.description}
                                                    className={`w-full flex items-center justify-between px-3.5 py-2.5 rounded-xl text-[12px] font-bold transition-all ${selectedMetricId === m.id
                                                        ? 'bg-sky-900 text-white shadow-lg'
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

                {/* Dynamic Weights Sliders Section */}
                {selectedMetric.isCalculated && (
                    <section className={`pt-6 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-200'}`}>
                        <div className="flex items-center justify-between mb-4">
                            <h4 className={`text-[13px] font-black uppercase tracking-[0.2em] flex items-center gap-1.5 ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>
                                <Activity className="w-3 h-3 text-sky-800" /> Dimension Weighting
                            </h4>
                            <button onClick={resetWeights} className="text-[13px] font-bold text-sky-800 hover:text-sky-700 uppercase tracking-widest transition-colors">
                                Reset
                            </button>
                        </div>
                        <div className="space-y-4 mb-6">
                            {FLAT_METRICS.filter(m => m.isContributory).map(m => (
                                <div key={m.id} className="space-y-2">
                                    <div className="flex justify-between items-center text-[12px] font-bold">
                                        <span className={isDarkMode ? 'text-neutral-400' : 'text-neutral-600'}>{m.icon} {m.label}</span>
                                        <span className="text-sky-800 font-black">{weights[m.id]?.toFixed(2)}</span>
                                    </div>
                                    <input
                                        type="range" min="0" max="1" step="0.05"
                                        value={weights[m.id] || 0}
                                        onChange={(e) => setWeights((prev: any) => ({ ...prev, [m.id]: parseFloat(e.target.value) }))}
                                        className="w-full h-1.5 bg-neutral-200 dark:bg-neutral-800 rounded-lg appearance-none cursor-pointer accent-sky-800"
                                    />
                                </div>
                            ))}
                        </div>

                        <div className={`p-4 rounded-2xl border ${isDarkMode ? 'bg-sky-800/5 border-sky-800/10' : 'bg-sky-50 border-sky-100'}`}>
                            <p className={`text-[12px] leading-relaxed mb-4 ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>
                                <strong className="text-sky-800 uppercase tracking-wider">AHP Methodology</strong><br />
                                Defining weights manually can be subjective and inconsistent. The <strong>Analytic Hierarchy Process</strong> uses pairwise comparisons to mathematically derive optimal weights while measuring your decision consistency.
                            </p>
                            <button
                                onClick={() => setIsAHPModalOpen(true)}
                                className="w-full py-2.5 rounded-xl bg-sky-900 hover:bg-sky-800 text-white text-[12px] font-bold uppercase tracking-widest shadow-lg shadow-sky-800/20 transition-all flex items-center justify-center gap-2"
                            >
                                Start AHP Survey
                            </button>
                        </div>
                    </section>
                )}
            </div>
        </div>
    );
};
