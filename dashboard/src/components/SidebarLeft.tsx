import React, { useState } from 'react';
import { Sun, Moon, Download, Info, ListFilter, ChevronDown, Activity, Languages } from 'lucide-react';
import { MetricDef } from '../types';
import { METRICS, FLAT_METRICS } from '../constants';
import { useTranslation } from 'react-i18next';
import { Tooltip } from './Tooltip';

interface SidebarLeftProps {
    isDarkMode: boolean;
    setIsDarkMode: (val: boolean) => void;
    setShowDownload: (val: boolean) => void;
    setShowAbout: (val: boolean) => void;
    selectedMetric: MetricDef;
    selectedMetricId: string;
    setSelectedMetricId: (val: string) => void;
    collapsedSections: Record<string, boolean>;
    toggleSection: (cat: string) => void;
    weights: Record<string, number>;
    setWeights: (val: any) => void;
    resetWeights: () => void;
    setIsAHPModalOpen: (val: boolean) => void;
    selectedVariations: Record<string, string>;
    setSelectedVariations: (val: any) => void;
    discoveredVariations: Record<string, string>[];
    selectedMode: { id: string };
    viewLevel: string;
    setSelectedDetailMetric: (m: MetricDef | null) => void;
}

export const SidebarLeft: React.FC<SidebarLeftProps> = ({
    isDarkMode, setIsDarkMode, setShowDownload, setShowAbout,
    selectedMetric, selectedMetricId, setSelectedMetricId,
    collapsedSections, toggleSection,
    weights, setWeights, resetWeights, setIsAHPModalOpen,
    selectedVariations, setSelectedVariations,
    discoveredVariations, selectedMode, viewLevel,
    setSelectedDetailMetric
}) => {
    const { t, i18n } = useTranslation();
    const toggleLanguage = () => {
        i18n.changeLanguage(i18n.language === 'en' ? 'pt' : 'en');
    };

    const [activeTab, setActiveTab] = useState<'index' | 'indicators'>('index');

    const categories = Object.keys(METRICS);
    const indexCategories = categories.slice(0, 2);
    const indicatorCategories = categories.slice(2);

    return (
        <div className={`absolute top-4 left-4 w-[380px] max-h-[calc(100vh-120px)] flex flex-col ${isDarkMode ? 'bg-neutral-900/95 border-neutral-800 shadow-2xl' : 'bg-white/95 border-neutral-200 shadow-2xl'} border rounded-[32px] backdrop-blur-xl z-[1001] transition-all overflow-hidden`}>
            <div className={`p-6 border-b ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>
                <div className="flex items-center gap-4 mb-6">
                    <img src="images/logo/icon.png" alt="Logo" className="w-12 h-12 object-contain" />
                    <div className="flex flex-col">
                        <h1 className="text-base font-black tracking-tighter uppercase leading-none">{t('common.impt_0')} <span className="text-sky-800">{t('common.impt_1')}</span> {t('common.impt_2')}</h1>
                        <p className={`text-[12px] font-bold uppercase tracking-widest opacity-40 mt-1`}>{t('common.lma')}</p>
                    </div>
                </div>

                <div className="flex items-center justify-between mb-4">
                    <div className="flex gap-1">
                        <Tooltip content={t('tooltips.toggle_theme')} isDarkMode={true}>
                            <button onClick={() => setIsDarkMode(!isDarkMode)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                {isDarkMode ? <Sun className="w-5 h-5" /> : <Moon className="w-5 h-5" />}
                            </button>
                        </Tooltip>
                        <Tooltip content={i18n.language === 'en' ? 'Português' : 'English'} isDarkMode={true}>
                            <button onClick={toggleLanguage} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                <Languages className="w-5 h-5" />
                            </button>
                        </Tooltip>
                        <Tooltip content={t('tooltips.download_data')} isDarkMode={true}>
                            <button onClick={() => setShowDownload(true)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                <Download className="w-5 h-5" />
                            </button>
                        </Tooltip>
                        <Tooltip content={t('tooltips.about')} isDarkMode={true}>
                            <button onClick={() => setShowAbout(true)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                <Info className="w-5 h-5" />
                            </button>
                        </Tooltip>
                    </div>
                    <div className={`px-2.5 py-1 rounded-lg text-[13px] font-black uppercase tracking-widest ${isDarkMode ? 'bg-neutral-800 text-neutral-500' : 'bg-neutral-100 text-neutral-400'}`}>{t('common.beta')}</div>
                </div>
            </div>

            {/* Tab Navigation */}
            <div className="px-6 py-2">
                <div className={`p-1 rounded-2xl flex gap-1 ${isDarkMode ? 'bg-neutral-800/50' : 'bg-neutral-100/50'} backdrop-blur-sm`}>
                    <button
                        onClick={() => setActiveTab('index')}
                        className={`flex-1 flex items-center justify-center gap-2 py-2.5 rounded-xl text-[12px] font-black uppercase tracking-widest transition-all duration-300 ${activeTab === 'index'
                            ? (isDarkMode ? 'bg-sky-800 text-white shadow-lg shadow-sky-900/20' : 'bg-white text-sky-900 shadow-sm')
                            : (isDarkMode ? 'text-neutral-500 hover:text-neutral-300' : 'text-neutral-400 hover:text-neutral-600')
                            }`}
                    >
                        <ListFilter className="w-3.5 h-3.5" />
                        {t('sidebar.index')}
                    </button>
                    <button
                        onClick={() => setActiveTab('indicators')}
                        className={`flex-1 flex items-center justify-center gap-2 py-2.5 rounded-xl text-[12px] font-black uppercase tracking-widest transition-all duration-300 ${activeTab === 'indicators'
                            ? (isDarkMode ? 'bg-sky-800 text-white shadow-lg shadow-sky-900/20' : 'bg-white text-sky-900 shadow-sm')
                            : (isDarkMode ? 'text-neutral-500 hover:text-neutral-300' : 'text-neutral-400 hover:text-neutral-600')
                            }`}
                    >
                        <Activity className="w-3.5 h-3.5" />
                        {t('sidebar.indicators')}
                    </button>
                </div>
            </div>

            <div className="flex-1 overflow-y-auto p-6 space-y-8 scrollbar-hide">
                <section>
                    <div className="space-y-4">
                        {(activeTab === 'index' ? indexCategories : indicatorCategories).map(cat => (
                            <div key={cat} className={`border rounded-2xl transition-all ${isDarkMode ? 'border-neutral-800 bg-neutral-800/10' : 'border-neutral-100 bg-neutral-50/30'} ${!collapsedSections[cat] ? 'ring-1 ring-sky-800/20' : ''}`}>
                                <button onClick={() => toggleSection(cat)}
                                    className={`w-full flex items-center justify-between px-4 py-3.5 text-[12px] font-black uppercase tracking-widest transition-colors ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-600'}`}
                                >
                                    <span className="flex items-center gap-2">
                                        <div className={`w-1.5 h-1.5 rounded-full ${selectedMetric.category === cat ? 'bg-sky-800 shadow-[0_0_8px_rgba(99,102,241,0.5)]' : 'bg-neutral-700'}`} />{t(cat)}
                                    </span>
                                    <ChevronDown className={`w-3.5 h-3.5 transition-transform duration-300 ${collapsedSections[cat] ? '' : 'rotate-180'}`} />
                                </button>
                                {!collapsedSections[cat] && (
                                    <div className={`p-2 space-y-1.5 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>                                        {METRICS[cat].map(m => {
                                        const isSelected = selectedMetricId === m.id;
                                        const validVariations = m.valid_id_variations || (isSelected ? discoveredVariations : []);

                                        return (
                                            <div key={m.id} className="space-y-1">
                                                <button onClick={() => {
                                                    setSelectedMetricId(m.id);

                                                    // Enforce valid variations for the new metric
                                                    if (validVariations && validVariations.length > 0) {
                                                        setSelectedVariations((prev: any) => {
                                                            const currentComb = Object.keys(m.id_variations || {}).reduce((acc, g) => {
                                                                const gDef = m.id_variations![g];
                                                                const opts = Array.isArray(gDef) ? gDef : gDef.options;
                                                                acc[g] = prev[g] || opts[0];
                                                                return acc;
                                                            }, {} as Record<string, string>);

                                                            const isValid = validVariations.some((validComb: any) => {
                                                                return Object.entries(currentComb).every(([k, v]) => validComb[k] === v);
                                                            });

                                                            if (!isValid) {
                                                                // Fallback to first valid combination for this metric
                                                                return { ...prev, ...validVariations[0] };
                                                            }
                                                            return prev;
                                                        });
                                                    }
                                                }}
                                                    className={`w-full group flex items-center justify-between px-3.5 py-2.5 rounded-xl text-[12px] font-bold transition-all ${isSelected
                                                        ? 'bg-sky-900 text-white shadow-lg shadow-sky-900/20'
                                                        : (isDarkMode ? 'hover:bg-neutral-800 text-neutral-500' : 'hover:bg-neutral-100 text-neutral-500')}`}
                                                >
                                                    <span className="flex items-center gap-3">
                                                        <span>{m.icon}</span>
                                                        <span className="truncate">{t(m.label)}</span>
                                                    </span>
                                                    <div 
                                                        onClick={(e) => {
                                                            e.stopPropagation();
                                                            setSelectedDetailMetric(m);
                                                        }}
                                                        className={`p-1 rounded-md transition-all ${isSelected ? 'hover:bg-white/10 text-white/40 hover:text-white' : 'hover:bg-neutral-700/50 text-neutral-600 hover:text-neutral-400 opacity-0 group-hover:opacity-100'}`}
                                                    >
                                                        <Info className="w-3.5 h-3.5" />
                                                    </div>
                                                </button>

                                                {isSelected && m.id_variations && (
                                                    <div className={`mt-2 mb-3 p-3 rounded-2xl space-y-3 ${isDarkMode ? 'bg-neutral-950/50' : 'bg-white/50 border border-neutral-100'}`}>
                                                        {Object.entries(m.id_variations).map(([group, optionsDef], index, arr) => {
                                                            if (!Array.isArray(optionsDef)) {
                                                                if (optionsDef.modes && !optionsDef.modes.includes(selectedMode.id as any)) return null;
                                                                if (optionsDef.viewLevels && !optionsDef.viewLevels.includes(viewLevel as any)) return null;
                                                            }
                                                            const options = Array.isArray(optionsDef) ? optionsDef : optionsDef.options;
                                                            let visibleOptions = options;
                                                            if (validVariations && validVariations.length > 0) {
                                                                const priorGroups = arr.slice(0, index).map(a => a[0]);
                                                                const validSubset = validVariations.filter((comb: any) => {
                                                                    return priorGroups.every(g => {
                                                                        const gDef = m.id_variations![g];
                                                                        const opts = Array.isArray(gDef) ? gDef : gDef.options;
                                                                        return comb[g] === (selectedVariations[g] || opts[0]);
                                                                    });
                                                                });
                                                                visibleOptions = options.filter(opt => {
                                                                    return validSubset.some((comb: any) => comb[group] === opt);
                                                                });
                                                            }
                                                            if (visibleOptions.length === 0) return null;

                                                            const isSlider = !Array.isArray(optionsDef) && optionsDef.formSlider;
                                                            const selectedOpt = selectedVariations[group] || options[0];

                                                            return (
                                                                <div key={group} className="space-y-1.5">
                                                                    <div className="flex justify-between items-center px-1">
                                                                        <label className={`text-[10px] font-black uppercase tracking-widest opacity-40 ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>
                                                                            {t(`variations.${group}`)}
                                                                        </label>
                                                                        {isSlider && (
                                                                            <span className="text-[11px] font-black text-sky-800">
                                                                                {t(`variations.${selectedOpt}`)}
                                                                            </span>
                                                                        )}
                                                                    </div>

                                                                    {isSlider ? (
                                                                        <div className="px-1 py-2">
                                                                            <input
                                                                                type="range"
                                                                                min="0"
                                                                                max={visibleOptions.length - 1}
                                                                                step="1"
                                                                                value={visibleOptions.indexOf(selectedOpt)}
                                                                                onChange={(e) => {
                                                                                    const index = parseInt(e.target.value);
                                                                                    const opt = visibleOptions[index];
                                                                                    setSelectedVariations((prev: any) => {
                                                                                        const next = { ...prev, [group]: opt };

                                                                                        if (validVariations && validVariations.length > 0) {
                                                                                            const currentComb = Object.keys(m.id_variations || {}).reduce((acc, g) => {
                                                                                                const gDef = m.id_variations![g];
                                                                                                const opts = Array.isArray(gDef) ? gDef : gDef.options;
                                                                                                acc[g] = next[g] || opts[0];
                                                                                                return acc;
                                                                                            }, {} as Record<string, string>);

                                                                                            const isValid = validVariations.some((validComb: any) => {
                                                                                                return Object.entries(currentComb).every(([k, v]) => validComb[k] === v);
                                                                                            });

                                                                                            if (!isValid) {
                                                                                                const fallbackComb = validVariations.find((validComb: any) => validComb[group] === opt);
                                                                                                if (fallbackComb) {
                                                                                                    return { ...prev, ...fallbackComb };
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                        return next;
                                                                                    });
                                                                                }}
                                                                                className="w-full h-1.5 bg-neutral-200 dark:bg-neutral-800 rounded-lg appearance-none cursor-pointer accent-sky-800"
                                                                            />
                                                                        </div>
                                                                    ) : (
                                                                        <div className="flex flex-wrap gap-1">
                                                                            {visibleOptions.map(opt => {
                                                                                const isOptSelected = selectedVariations[group] === opt || (!selectedVariations[group] && options[0] === opt);
                                                                                return (
                                                                                    <button
                                                                                        key={opt}
                                                                                        onClick={() => {
                                                                                            setSelectedVariations((prev: any) => {
                                                                                                const next = { ...prev, [group]: opt };

                                                                                                if (validVariations && validVariations.length > 0) {
                                                                                                    const currentComb = Object.keys(m.id_variations || {}).reduce((acc, g) => {
                                                                                                        const gDef = m.id_variations![g];
                                                                                                        const opts = Array.isArray(gDef) ? gDef : gDef.options;
                                                                                                        acc[g] = next[g] || opts[0];
                                                                                                        return acc;
                                                                                                    }, {} as Record<string, string>);

                                                                                                    const isValid = validVariations.some((validComb: any) => {
                                                                                                        return Object.entries(currentComb).every(([k, v]) => validComb[k] === v);
                                                                                                    });

                                                                                                    if (!isValid) {
                                                                                                        // Find the FIRST valid combination that includes the newly selected option
                                                                                                        const fallbackComb = validVariations.find((validComb: any) => validComb[group] === opt);
                                                                                                        if (fallbackComb) {
                                                                                                            return { ...prev, ...fallbackComb };
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                                return next;
                                                                                            });
                                                                                        }}
                                                                                        className={`px-3 py-1.5 rounded-lg text-[11px] font-bold transition-all ${isOptSelected
                                                                                            ? 'bg-sky-800 text-white shadow-sm'
                                                                                            : (isDarkMode ? 'bg-neutral-800 text-neutral-500 hover:text-neutral-300' : 'bg-neutral-100 text-neutral-500 hover:bg-neutral-200')}`}
                                                                                    >
                                                                                        {t(`variations.${opt}`)}
                                                                                    </button>
                                                                                );
                                                                            })}
                                                                        </div>
                                                                    )}
                                                                </div>
                                                            );;
                                                        })}
                                                    </div>
                                                )}
                                            </div>
                                        );
                                    })}
                                    </div>
                                )}

                                {/* Dynamic Weights Sliders Section (Only for Index -> Mobility Poverty Index) */}
                                {activeTab === 'index' && cat === 'metrics.categories.mobility_poverty_index' && selectedMetric.isCalculated && (
                                    <section className={`p-6 border-t ${isDarkMode ? 'border-neutral-800' : 'border-neutral-200'}`}>
                                        <div className="flex items-center justify-between mb-4">
                                            <h4 className={`text-[13px] font-black uppercase tracking-[0.2em] flex items-center gap-1.5 ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>
                                                <Activity className="w-3 h-3 text-sky-800" /> {t('sidebar.dimension_weighting')}
                                            </h4>
                                            <button onClick={resetWeights} className="text-[13px] font-bold text-sky-800 hover:text-sky-700 uppercase tracking-widest transition-colors">
                                                {t('common.reset')}
                                            </button>
                                        </div>
                                        <div className="space-y-4 mb-6">
                                            {FLAT_METRICS.filter(m => m.isContributory).map(m => (
                                                <div key={m.id} className="space-y-2">
                                                    <div className="flex justify-between items-center text-[12px] font-bold">
                                                        <span className={isDarkMode ? 'text-neutral-400' : 'text-neutral-600'}>{m.icon} {t(m.label)}</span>
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
                                                <strong className="text-sky-800 uppercase tracking-wider">{t('sidebar.ahp_methodology')}</strong><br />
                                                {(() => {
                                                    const text = t('sidebar.ahp_description');
                                                    return text.replace('<1>', '').replace('</1>', '');
                                                })()}
                                            </p>
                                            <button
                                                onClick={() => setIsAHPModalOpen(true)}
                                                className="w-full py-2.5 rounded-xl bg-sky-900 hover:bg-sky-800 text-white text-[12px] font-bold uppercase tracking-widest shadow-lg shadow-sky-800/20 transition-all flex items-center justify-center gap-2"
                                            >
                                                {t('sidebar.start_ahp')}
                                            </button>
                                        </div>
                                    </section>
                                )}
                            </div>
                        ))}
                    </div>
                </section>
            </div>
        </div>
    );
};
