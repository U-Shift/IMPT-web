import React, { useState } from 'react';
import { Sun, Moon, Download, Info, ListFilter, ChevronDown, Activity, Languages, Eye, EyeOff, Layers, Menu, X, Globe, HelpCircle } from 'lucide-react';
import { MetricDef } from '../types';
import { METRICS, FLAT_METRICS, REGION_KEYS, REGIONS, MODES } from '../constants';
import { useTranslation } from 'react-i18next';
import { Tooltip } from './Tooltip';
import { MapFilterDropdown } from './MapFilterDropdown';
import { ModeSelector } from './ModeSelector';
import { MapTools } from './MapTools';

interface SidebarLeftProps {
    isDarkMode: boolean;
    setIsDarkMode: (val: boolean) => void;
    isColorBlindMode: boolean;
    setIsColorBlindMode: (val: boolean) => void;
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
    showBuiltArea: boolean;
    setShowBuiltArea: (val: boolean) => void;
    isMobile?: boolean;
    isOpen?: boolean;
    setIsOpen?: (val: boolean) => void;
    nutFilter?: any;
    setNutFilter?: (val: any) => void;
    setViewLevel?: (val: any) => void;
    setSelectedModeId?: (val: any) => void;
    isMetricAvailable?: (m: string, l: string, mo?: any) => boolean;
    isModeAvailable?: (mo: string, m: string, l: string) => boolean;
    mapStyle: string;
    setMapStyle: (val: string) => void;
    onZoomIn?: () => void;
    onZoomOut?: () => void;
    onDownloadAHP?: () => void;
}

const AuxiliaryDataRenderer: React.FC<{
    url: string;
    metricId: string;
    t: (key: string) => string;
    render: (data: any, metricId: string, t: (key: string) => string) => React.ReactNode;
}> = ({ url, metricId, t, render }) => {
    const [data, setData] = useState<any>(null);
    const [loading, setLoading] = useState(false);

    React.useEffect(() => {
        setLoading(true);
        fetch(url)
            .then(res => res.json())
            .then(json => {
                setData(json);
                setLoading(false);
            })
            .catch(err => {
                console.error("Error fetching auxiliary data:", err);
                setLoading(false);
            });
    }, [url]);

    if (loading) return (
        <div className="mt-2 p-3 animate-pulse bg-neutral-500/10 rounded-xl h-20 flex items-center justify-center">
            <div className="w-5 h-5 border-2 border-sky-800 border-t-transparent rounded-full animate-spin" />
        </div>
    );
    if (!data) return null;

    return <>{render(data, metricId, t)}</>;
};

export const SidebarLeft: React.FC<SidebarLeftProps> = ({
    isDarkMode, setIsDarkMode, isColorBlindMode, setIsColorBlindMode, setShowDownload, setShowAbout,
    startTutorial,
    selectedMetric, selectedMetricId, setSelectedMetricId,
    collapsedSections, toggleSection,
    weights, setWeights, resetWeights, setIsAHPModalOpen,
    selectedVariations, setSelectedVariations,
    discoveredVariations, selectedMode, viewLevel,
    showBuiltArea, setShowBuiltArea,
    isMobile, isOpen, setIsOpen,
    nutFilter, setNutFilter, setViewLevel, setSelectedModeId,
    isMetricAvailable, isModeAvailable,
    mapStyle, setMapStyle,
    onZoomIn, onZoomOut, onDownloadAHP
}) => {
    const { t, i18n } = useTranslation();
    const toggleLanguage = () => {
        i18n.changeLanguage(i18n.language === 'en' ? 'pt' : 'en');
    };

    const [activeTab, setActiveTab] = useState<'index' | 'indicators'>('index');

    const categories = Object.keys(METRICS);
    const indexCategories = categories.slice(0, 2);
    const indicatorCategories = categories.slice(2);

    const handleToggle = () => setIsOpen?.(!isOpen);

    return (
        <>
            {/* Mobile Top Controls */}
            {isMobile && (
                <div className="fixed top-4 left-4 z-[1002] flex flex-col gap-3 pointer-events-none">
                    <div className={`p-4 rounded-[24px] shadow-xl backdrop-blur-md border pointer-events-auto ${isDarkMode ? 'bg-neutral-900/90 border-neutral-800' : 'bg-white/90 border-neutral-200'}`}>
                        <div className="flex items-center gap-3">
                            <img src="images/logo/icon.png" alt="Logo" className="w-10 h-10 object-contain" />
                            <div className="flex flex-col">
                                <h1 className="text-sm font-black tracking-tighter uppercase leading-none">{t('common.impt_0')} <span className="text-sky-800">{t('common.impt_1')}</span> {t('common.impt_2')}</h1>
                                <p className={`text-[10px] font-bold uppercase tracking-widest opacity-40 mt-1`}>{t('common.lma')}</p>
                            </div>
                        </div>
                    </div>
                    <button
                        onClick={handleToggle}
                        className={`p-4 rounded-2xl shadow-xl backdrop-blur-md transition-all self-start pointer-events-auto ${isDarkMode ? 'bg-neutral-900/90 text-white' : 'bg-white/90 text-neutral-900'}`}
                    >
                        {isOpen ? <X className="w-6 h-6" /> : <Menu className="w-6 h-6" />}
                    </button>
                </div>
            )}

            {/* Always Visible Mode Selector on Mobile */}
            {isMobile && !isOpen && (
                <div className="fixed top-[168px] left-4 z-[1002]">
                    <ModeSelector
                        value={selectedMode.id}
                        isDark={isDarkMode}
                        options={MODES.filter(m => isModeAvailable?.(m.id, selectedMetricId, viewLevel))
                            .map(m => ({ id: m.id, label: t(m.label), icon: m.icon }))}
                        onChange={(id) => setSelectedModeId?.(id)}
                    />
                </div>
            )}

            <div data-tour="sidebar-left" className={`
                ${isMobile ? 'fixed inset-0 w-full max-h-none rounded-none' : 'absolute top-4 left-4 w-[380px] max-h-[calc(100vh-120px)] rounded-[32px]'}
                flex flex-col ${isDarkMode ? 'bg-neutral-900/95 border-neutral-800 shadow-2xl' : 'bg-white/95 border-neutral-200 shadow-2xl'} 
                border backdrop-blur-xl z-[1001] transition-all duration-300 overflow-hidden
                ${isMobile && !isOpen ? '-translate-x-full' : 'translate-x-0'}
            `}>
                <div className={`p-6 border-b ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'} ${isMobile ? 'pt-40' : ''}`}>
                    {!isMobile && (
                        <div className="flex items-center gap-4 mb-6">
                            <img src="images/logo/icon.png" alt="Logo" className="w-12 h-12 object-contain" />
                            <div className="flex flex-col">
                                <h1 className="text-base font-black tracking-tighter uppercase leading-none">{t('common.impt_0')} <span className="text-sky-800">{t('common.impt_1')}</span> {t('common.impt_2')}</h1>
                                <p className={`text-[12px] font-bold uppercase tracking-widest opacity-40 mt-1`}>{t('common.lma')}</p>
                            </div>
                        </div>
                    )}

                    <div data-tour="top-controls" className="flex items-center justify-between mb-0">
                        <div className="flex gap-1">
                            <Tooltip content={t('tooltips.toggle_theme')} isDarkMode={true}>
                                <button data-tour="theme-toggle" onClick={() => setIsDarkMode(!isDarkMode)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                    {isDarkMode ? <Sun className="w-5 h-5" /> : <Moon className="w-5 h-5" />}
                                </button>
                            </Tooltip>
                            <Tooltip content={t('tooltips.toggle_colorblind')} isDarkMode={true}>
                                <button data-tour="colorblind-toggle" onClick={() => setIsColorBlindMode(!isColorBlindMode)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                    {isColorBlindMode ? <Eye className="w-5 h-5 text-sky-600" /> : <EyeOff className="w-5 h-5" />}
                                </button>
                            </Tooltip>
                            <Tooltip content={i18n.language === 'en' ? 'Português' : 'English'} isDarkMode={true}>
                                <button data-tour="language-toggle" onClick={toggleLanguage} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                    <Languages className="w-5 h-5" />
                                </button>
                            </Tooltip>
                            <Tooltip content={t('tooltips.download_data')} isDarkMode={true}>
                                <button data-tour="download-modal-btn" onClick={() => setShowDownload(true)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                    <Download className="w-5 h-5" />
                                </button>
                            </Tooltip>
                            <Tooltip content={t('tooltips.about')} isDarkMode={true}>
                                <button data-tour="about-modal-btn" onClick={() => setShowAbout(true)} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                    <Info className="w-5 h-5" />
                                </button>
                            </Tooltip>
                            <Tooltip content={t('tooltips.tutorial')} isDarkMode={true}>
                                <button data-tour="tutorial-btn" onClick={startTutorial} className={`p-2.5 rounded-xl transition-all ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-400' : 'hover:bg-neutral-100 text-neutral-500'}`}>
                                    <HelpCircle className="w-5 h-5" />
                                </button>
                            </Tooltip>
                        </div>
                        {!isMobile && <div className={`px-2.5 py-1 rounded-lg text-[13px] font-black uppercase tracking-widest ${isDarkMode ? 'bg-neutral-800 text-neutral-500' : 'bg-neutral-100 text-neutral-400'}`}>{t('common.beta')}</div>}
                    </div>
                </div>

                <div className={`flex-1 flex flex-col ${isMobile ? 'overflow-y-auto' : 'overflow-hidden'} scrollbar-hide`}>
                    {/* Tab Navigation */}
                    {
                        isMobile && (
                            <div className="px-6 py-2 mt-6">
                                <h4 className={`text-[12px] font-black uppercase tracking-[0.2em] ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>
                                    {t('sidebar.map_layers')}
                                </h4>
                            </div>
                        )
                    }
                    <div className="px-6 py-4">
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
                                data-tour="tab-indicators"
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

                    <div className={`flex-1 ${isMobile ? '' : 'overflow-y-auto'} p-6 space-y-8 scrollbar-hide`}>
                        <section>
                            <div className="space-y-4">
                                {(activeTab === 'index' ? indexCategories : indicatorCategories).map(cat => (
                                    <div key={cat} data-tour={`section-${cat}`} className={`border rounded-2xl transition-all ${isDarkMode ? 'border-neutral-800 bg-neutral-800/10' : 'border-neutral-100 bg-neutral-50/30'} ${!collapsedSections[cat] ? 'ring-1 ring-sky-800/20' : ''}`}>
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
                                                            if (isMobile) setIsOpen?.(false);
                                                        }}
                                                            className={`w-full flex items-center justify-between px-3.5 py-2.5 rounded-xl text-[12px] font-bold transition-all ${isSelected
                                                                ? 'bg-sky-900 text-white shadow-lg'
                                                                : (isDarkMode ? 'hover:bg-neutral-800 text-neutral-500' : 'hover:bg-neutral-100 text-neutral-500')}`}
                                                        >
                                                            <span className="flex items-center gap-3">
                                                                <span>{m.icon}</span>
                                                                <span className="truncate">{t(m.label)}</span>
                                                            </span>
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
                                                                    if (visibleOptions.length == 0 || (optionsDef as any).visible === false) return null;

                                                                    const isSlider = !Array.isArray(optionsDef) && optionsDef.formSlider;
                                                                    const selectedOpt = selectedVariations[group] || options[0];

                                                                    return (
                                                                        <div key={group} className={`space-y-1.5 ${isSlider ? 'pt-2' : ''} ${group === 'n_transfers' ? '-mt-1' : ''}`}>
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

                                                        {isSelected && m.auxiliaryDataUrl && m.renderAuxiliaryData && (
                                                            <div className="mt-1 px-1">
                                                                <AuxiliaryDataRenderer
                                                                    url={m.auxiliaryDataUrl}
                                                                    metricId={m.id}
                                                                    t={t}
                                                                    render={m.renderAuxiliaryData as any}
                                                                />
                                                            </div>
                                                        )}
                                                    </div>
                                                );
                                            })}
                                            </div>
                                        )}

                                        {/* Dynamic Weights Sliders Section (Only for Index -> Mobility Poverty Index) */}
                                        {activeTab === 'index' && cat === 'metrics.categories.mobility_poverty_index' && !collapsedSections[cat] && selectedMetric.isCalculated && (
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

                                                {onDownloadAHP && (
                                                    <button
                                                        onClick={onDownloadAHP}
                                                        className={`w-full mb-4 py-2.5 rounded-xl ${isDarkMode ? 'bg-neutral-800 hover:bg-neutral-700 text-neutral-300' : 'bg-neutral-100 hover:bg-neutral-200 text-neutral-600'} text-[12px] font-bold uppercase tracking-widest transition-all flex items-center justify-center gap-2`}
                                                    >
                                                        <Download className="w-4 h-4" />
                                                        {t('tooltips.download_results')} (CSV)
                                                    </button>
                                                )}

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

                    {/* Mobile Map Settings Section */}
                    {isMobile && (
                        <section className="px-6 py-4">
                            <h4 className={`text-[12px] font-black uppercase tracking-[0.2em] ${isDarkMode ? 'text-neutral-500' : 'text-neutral-400'}`}>
                                {t('sidebar.map_settings')}
                            </h4>
                            <div className="flex flex-col gap-4 mt-4">
                                <div className="flex flex-col gap-4">
                                    <div className="flex w-full">
                                        <MapFilterDropdown
                                            isMobile={isMobile}
                                            label={t('map.view_level')}
                                            value={viewLevel}
                                            isDark={isDarkMode}
                                            icon={<Layers className="w-3.5 h-3.5" />}
                                            options={(['hex', 'freguesia', 'municipality'] as const)
                                                .filter(l => isMetricAvailable?.(selectedMetricId, l, selectedMode))
                                                .map(l => ({ id: l, label: l === 'hex' ? t('map.grid') : t(`map.${l}`) }))}
                                            onChange={(id) => {
                                                setViewLevel?.(id);
                                                if (isMobile) setIsOpen?.(false);
                                            }}
                                        />
                                    </div>
                                    <div className="w-full">
                                        <MapFilterDropdown
                                            isMobile={isMobile}
                                            label={t('map.region')}
                                            value={nutFilter}
                                            isDark={isDarkMode}
                                            icon={<Globe className="w-3.5 h-3.5" />}
                                            options={REGION_KEYS.map(n => ({ id: n, label: t(REGIONS[n].name) }))}
                                            onChange={(id) => {
                                                setNutFilter?.(id);
                                                if (isMobile) setIsOpen?.(false);
                                            }}
                                        />
                                    </div>
                                    <MapTools
                                        isMobile={true}
                                        isDarkMode={isDarkMode}
                                        mapStyle={mapStyle}
                                        setMapStyle={setMapStyle}
                                        showBuiltArea={showBuiltArea}
                                        setShowBuiltArea={setShowBuiltArea}
                                        onZoomIn={onZoomIn}
                                        onZoomOut={onZoomOut}
                                        onChange={() => {
                                            if (isMobile) setIsOpen?.(false);
                                        }}
                                    />
                                </div>
                            </div>
                        </section>
                    )}
                </div>
            </div>
        </>
    );
};
