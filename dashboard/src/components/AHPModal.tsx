import React, { useState, useMemo } from 'react';
import { X, ChevronRight, ChevronLeft, Check, AlertTriangle } from 'lucide-react';
import { MetricDef } from '../types';
import { useTranslation } from 'react-i18next';
import { AHP_SURVEY_URL } from '../constants';

interface AHPModalProps {
    metrics: MetricDef[];
    isOpen: boolean;
    onClose: () => void;
    onApplyWeights: (weights: Record<string, number>) => void;
    isDarkMode: boolean;
}

// Maps slider [-8, 8] to Saaty Scale (1 to 9)
const getSaatyValue = (sliderValue: number): number => {
    if (sliderValue === 0) return 1;
    if (sliderValue < 0) return Math.abs(sliderValue) + 1;
    return 1 / (sliderValue + 1);
};

export const AHPModal: React.FC<AHPModalProps> = ({ metrics, isOpen, onClose, onApplyWeights, isDarkMode }) => {
    const { t } = useTranslation();
    // Generate Pairwise Combinations
    const pairs = useMemo(() => {
        const result = [];
        for (let i = 0; i < metrics.length; i++) {
            for (let j = i + 1; j < metrics.length; j++) {
                result.push([i, j]);
            }
        }
        return result;
    }, [metrics]);

    // Slider state (-8 to +8) for each pair
    const [selections, setSelections] = useState<number[]>(new Array(pairs.length).fill(0));
    const [currentStep, setCurrentStep] = useState(0);
    const [forceConsistency, setForceConsistency] = useState(true);

    // Research Submission state
    const [role, setRole] = useState<string>('');
    const [otherRole, setOtherRole] = useState<string>('');
    const [isSubmitting, setIsSubmitting] = useState(false);
    const [submitSuccess, setSubmitSuccess] = useState(false);
    const [submitError, setSubmitError] = useState(false);

    // Calculate AHP Weights & Consistency
    const results = useMemo(() => {
        if (currentStep !== pairs.length) return null;

        const n = metrics.length;
        const matrix = Array.from({ length: n }, () => new Array(n).fill(1));

        // Fill matrix
        pairs.forEach(([i, j], idx) => {
            const val = getSaatyValue(selections[idx]);
            matrix[i][j] = val;
            matrix[j][i] = 1 / val;
        });

        // Calculate Column Sums
        const colSums = new Array(n).fill(0);
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                colSums[j] += matrix[i][j];
            }
        }

        // Normalize matrix & get row averages (weights)
        const weights = new Array(n).fill(0);
        for (let i = 0; i < n; i++) {
            let rowSum = 0;
            for (let j = 0; j < n; j++) {
                rowSum += matrix[i][j] / colSums[j];
            }
            weights[i] = rowSum / n;
        }

        // Calculate Consistency Ratio (CR)
        let lambdaMax = 0;
        for (let i = 0; i < n; i++) {
            let weightedSum = 0;
            for (let j = 0; j < n; j++) {
                weightedSum += matrix[i][j] * weights[j];
            }
            lambdaMax += weightedSum / weights[i];
        }
        lambdaMax /= n;

        // Dynamic RI Table
        let CR = 0;
        if (n > 2) {
            const CI = (lambdaMax - n) / (n - 1);
            const RI_TABLE: Record<number, number> = {
                1: 0.00, 2: 0.00, 3: 0.58, 4: 0.90, 5: 1.12,
                6: 1.24, 7: 1.32, 8: 1.41, 9: 1.45, 10: 1.49
            };
            const RI = RI_TABLE[n] || 1.49;
            CR = CI / RI;
        }

        const inconsistencyScores = pairs.map(([i, j], idx) => {
            const userVal = getSaatyValue(selections[idx]);
            const weightRatio = weights[i] / weights[j];
            const deviation = Math.max(userVal / weightRatio, weightRatio / userVal);
            return { index: idx, deviation, pair: [i, j] };
        }).sort((a, b) => b.deviation - a.deviation);

        return { weights, CR, inconsistentPairs: inconsistencyScores.slice(0, 2) };
    }, [currentStep, pairs, selections, metrics.length]);

    if (!isOpen) return null;

    const isComplete = currentStep === pairs.length;

    const handleApply = () => {
        if (results) {
            const weightsRecord: Record<string, number> = {};
            metrics.forEach((m, idx) => {
                weightsRecord[m.id] = results.weights[idx];
            });
            onApplyWeights(weightsRecord);
        }
    };

    const GOOGLE_SCRIPT_URL = AHP_SURVEY_URL;

    const handleSubmitResearch = async () => {
        if (!results) return;
        setIsSubmitting(true);
        setSubmitError(false);

        const finalRole = role === 'other' ? otherRole : role;

        // Prepare data object based on results
        const data: Record<string, any> = {
            role: finalRole,
            CR: results.CR,
            timestamp: new Date().toISOString()
        };

        metrics.forEach((m, idx) => {
            data[`weight_${m.id}`] = results.weights[idx];
        });

        try {
            await fetch(GOOGLE_SCRIPT_URL, {
                method: 'POST',
                mode: 'no-cors',
                headers: {
                    'Content-Type': 'text/plain;charset=utf-8',
                },
                body: JSON.stringify(data)
            });
            setSubmitSuccess(true);
        } catch (error) {
            console.error("Error submitting AHP results:", error);
            setSubmitError(true);
        } finally {
            setIsSubmitting(false);
        }
    };

    const handleSliderChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const newVal = parseInt(e.target.value);
        const newSelections = [...selections];
        newSelections[currentStep] = newVal;
        setSelections(newSelections);
    };

    // Label mapping for the slider values
    const getIntensityLabel = (val: number) => {
        const abs = Math.abs(val);
        if (abs === 0) return t('ahp.intensity.equal');
        if (abs <= 2) return t('ahp.intensity.slight');
        if (abs <= 4) return t('ahp.intensity.moderate');
        if (abs <= 6) return t('ahp.intensity.strong');
        return t('ahp.intensity.extreme');
    };

    return (
        <div className="fixed inset-0 z-[99999] lg:flex lg:items-center lg:justify-center p-0 lg:p-4">
            <div className="absolute inset-0 bg-black/60 backdrop-blur-sm" onClick={onClose} />
            <div className={`
                ${isDarkMode ? 'bg-neutral-900' : 'bg-white'} 
                lg:border ${isDarkMode ? 'lg:border-neutral-800' : 'lg:border-neutral-200'}
                relative w-full h-[100dvh] lg:h-auto lg:max-h-[90vh] lg:max-w-2xl flex flex-col overflow-hidden lg:rounded-3xl shadow-2xl transition-all
            `} onClick={e => e.stopPropagation()}>

                {/* Header */}
                <div className={`flex items-center justify-between p-5 lg:p-6 border-b shrink-0 ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>
                    <div className="flex items-center gap-4">
                        <div>
                            <h2 className="text-xl lg:text-2xl font-black uppercase tracking-widest">{t('ahp.survey_title')}</h2>
                            <p className={`text-sm lg:text-base mt-1 ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>
                                {t('ahp.survey_subtitle')}
                            </p>
                        </div>
                        <div className="h-8 w-px bg-neutral-200 dark:bg-neutral-800 hidden lg:block" />
                        <label className="flex items-center gap-2 cursor-pointer group">
                            <div
                                onClick={() => setForceConsistency(!forceConsistency)}
                                className={`w-10 h-5 rounded-full relative transition-colors ${forceConsistency ? 'bg-sky-900' : (isDarkMode ? 'bg-neutral-800' : 'bg-neutral-200')}`}
                            >
                                <div className={`absolute top-1 left-1 w-3 h-3 bg-white rounded-full transition-transform ${forceConsistency ? 'translate-x-5' : ''}`} />
                            </div>
                            <span className="text-[10px] lg:text-[12px] font-bold uppercase tracking-wider text-neutral-500 group-hover:text-sky-800 transition-colors hidden lg:inline">{t('ahp.force_consistency')}</span>
                        </label>
                    </div>
                    <button onClick={onClose} className={`p-2 rounded-full transition-colors ${isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-100'}`}>
                        <X className="w-6 h-6" />
                    </button>
                </div>

                {/* Content */}
                <div className="p-6 lg:p-8 flex-1 overflow-y-auto min-h-[300px]">
                    {!isComplete ? (
                        <div className="space-y-8 lg:space-y-12">
                            <div className="text-center space-y-2">
                                <p className={`text-[10px] lg:text-[12px] font-black uppercase tracking-widest ${isDarkMode ? 'text-sky-700' : 'text-sky-900'}`}>
                                    {t('ahp.comparison_step', { current: currentStep + 1, total: pairs.length })}
                                </p>
                                <h3 className="text-base lg:text-lg font-medium">{t('ahp.question')}</h3>
                            </div>

                            <div className="flex flex-col lg:flex-row items-center justify-between gap-4 lg:gap-6">
                                <div className={`w-full lg:flex-1 text-center lg:text-right p-4 rounded-xl border-2 transition-colors ${selections[currentStep] < 0 ? 'border-sky-800 bg-sky-800/10 text-sky-800' : (isDarkMode ? 'border-neutral-800' : 'border-neutral-100')}`}>
                                    <p className="font-bold text-sm lg:text-base uppercase tracking-wider">{t(metrics[pairs[currentStep][0]].label)}</p>
                                    <p className="text-[10px] lg:text-[12px] opacity-60 mt-1">{t(metrics[pairs[currentStep][0]].description || '')}</p>
                                </div>
                                <div className="hidden lg:block text-neutral-400 font-black italic">VS</div>
                                <div className={`w-full lg:flex-1 text-center lg:text-left p-4 rounded-xl border-2 transition-colors ${selections[currentStep] > 0 ? 'border-sky-800 bg-sky-800/10 text-sky-800' : (isDarkMode ? 'border-neutral-800' : 'border-neutral-100')}`}>
                                    <p className="font-bold text-sm lg:text-base uppercase tracking-wider">{t(metrics[pairs[currentStep][1]].label)}</p>
                                    <p className="text-[10px] lg:text-[12px] opacity-60 mt-1">{t(metrics[pairs[currentStep][1]].description || '')}</p>
                                </div>
                            </div>

                            <div className="space-y-6">
                                <input
                                    type="range"
                                    min="-8" max="8" step="1"
                                    value={selections[currentStep]}
                                    onChange={handleSliderChange}
                                    className={`w-full h-2 rounded-lg appearance-none cursor-pointer accent-sky-800 outline-none ${isDarkMode ? 'bg-neutral-800' : 'bg-neutral-200'}`}
                                />
                                <div className="text-center h-6">
                                    <span className={`inline-block px-3 py-1 rounded-full text-[10px] lg:text-[12px] font-bold uppercase tracking-widest ${selections[currentStep] !== 0 ? 'bg-sky-800 text-white' : (isDarkMode ? 'bg-neutral-800 text-neutral-400' : 'bg-neutral-100 text-neutral-500')}`}>
                                        {getIntensityLabel(selections[currentStep])}
                                    </span>
                                </div>
                            </div>
                        </div>
                    ) : (
                        <div className="space-y-6 lg:space-y-8 animate-in fade-in zoom-in duration-300">
                            <div className="text-center space-y-2">
                                <div className="w-12 h-12 lg:w-16 lg:h-16 bg-green-500/20 text-green-500 rounded-full flex items-center justify-center mx-auto mb-4">
                                    <Check className="w-6 h-6 lg:w-8 lg:h-8" />
                                </div>
                                <h3 className="text-xl lg:text-2xl font-black uppercase tracking-widest">{t('ahp.complete')}</h3>
                                <p className={`text-sm lg:text-base ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>{t('ahp.complete_desc')}</p>
                            </div>

                            <div className="space-y-4">
                                {metrics.map((m, idx) => {
                                    const w = results?.weights[idx] || 0;
                                    const pct = (w * 100).toFixed(1);
                                    return (
                                        <div key={m.id} className="relative">
                                            <div className="flex justify-between text-[10px] lg:text-[12px] font-bold uppercase tracking-wider mb-1">
                                                <span>{t(m.label)}</span>
                                                <span>{pct}%</span>
                                            </div>
                                            <div className={`h-2 rounded-full overflow-hidden ${isDarkMode ? 'bg-neutral-800' : 'bg-neutral-100'}`}>
                                                <div className="h-full bg-sky-800 transition-all duration-1000 ease-out" style={{ width: `${pct}%` }} />
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>

                            {results && results.CR > 0.1 && (
                                <div className={`flex flex-col gap-4 p-4 rounded-xl ${forceConsistency ? 'bg-red-500/10 text-red-500 border border-red-500/20' : 'bg-amber-500/10 text-amber-500'}`}>
                                    <div className="flex items-start gap-3">
                                        <AlertTriangle className="w-5 h-5 shrink-0 mt-0.5" />
                                        <div className="text-sm lg:text-base">
                                            <p className="font-bold uppercase tracking-wider mb-1">
                                                {forceConsistency ? t('ahp.inconsistency_title') : t('ahp.inconsistency_answers')} (CR: {(results.CR).toFixed(2)})
                                            </p>
                                            <p className="opacity-80">
                                                {forceConsistency
                                                    ? t('ahp.inconsistency_desc_force')
                                                    : t('ahp.inconsistency_desc')}
                                            </p>
                                        </div>
                                    </div>

                                    <div className={`p-3 rounded-lg ${isDarkMode ? 'bg-neutral-800/50' : 'bg-white/50'} space-y-2`}>
                                        <p className="text-[10px] font-black uppercase tracking-widest opacity-60">{t('ahp.inconsistent_pairs_label')}:</p>
                                        {results.inconsistentPairs.map((item, idx) => (
                                            <div key={idx} className="flex items-center justify-between text-[10px] lg:text-[12px] group gap-2">
                                                <span className="font-bold truncate">
                                                    {t(metrics[item.pair[0]].label)} vs {t(metrics[item.pair[1]].label)}
                                                </span>
                                                <button
                                                    onClick={() => setCurrentStep(item.index)}
                                                    className="px-2 py-1 rounded bg-sky-800 text-white font-black uppercase tracking-tighter hover:bg-sky-700 transition-colors shrink-0"
                                                >
                                                    {t('common.fix')}
                                                </button>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            )}

                            {results && results.CR <= 0.1 && (
                                <div className={`flex flex-col gap-4 p-5 rounded-xl border ${isDarkMode ? 'bg-neutral-800/30 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                                    <div className="space-y-1">
                                        <h4 className="text-sm lg:text-base font-bold uppercase tracking-wider">{t('ahp.submit_research')}</h4>
                                        <p className="text-[10px] lg:text-[12px] opacity-70">{t('ahp.submit_research_desc')}</p>
                                    </div>

                                    {submitSuccess ? (
                                        <div className="flex items-center gap-2 text-green-500 font-bold text-[10px] lg:text-[12px] uppercase tracking-wider p-2">
                                            <Check className="w-4 h-4" /> {t('ahp.submit_success')}
                                        </div>
                                    ) : (
                                        <div className="space-y-4">
                                            <div className="space-y-2">
                                                <label className="text-[10px] lg:text-[12px] font-bold uppercase tracking-widest opacity-70">{t('ahp.role_label')}</label>
                                                <select
                                                    value={role}
                                                    onChange={(e) => setRole(e.target.value)}
                                                    disabled={isSubmitting}
                                                    className={`w-full p-2.5 rounded-lg text-sm outline-none border transition-colors focus:border-sky-800 ${isDarkMode ? 'bg-neutral-900 border-neutral-800' : 'bg-white border-neutral-200'}`}
                                                >
                                                    <option value="">-- {t('ahp.role_label')} --</option>
                                                    <option value="student">{t('ahp.role_student')}</option>
                                                    <option value="researcher">{t('ahp.role_researcher')}</option>
                                                    <option value="professional">{t('ahp.role_professional')}</option>
                                                    <option value="citizen">{t('ahp.role_citizen')}</option>
                                                    <option value="other">{t('ahp.role_other')}</option>
                                                </select>
                                            </div>
                                            {role === 'other' && (
                                                <div className="space-y-2 animate-in slide-in-from-top-2 duration-200">
                                                    <input
                                                        type="text"
                                                        value={otherRole}
                                                        onChange={(e) => setOtherRole(e.target.value)}
                                                        disabled={isSubmitting}
                                                        placeholder={t('ahp.role_other_placeholder')}
                                                        className={`w-full p-2.5 rounded-lg text-sm outline-none border transition-colors focus:border-sky-800 ${isDarkMode ? 'bg-neutral-900 border-neutral-800' : 'bg-white border-neutral-200'}`}
                                                    />
                                                </div>
                                            )}
                                            {submitError && (
                                                <div className="text-red-500 text-[10px] lg:text-[12px] font-bold uppercase tracking-wider p-1">
                                                    {t('ahp.submit_error')}
                                                </div>
                                            )}
                                            <button
                                                onClick={handleSubmitResearch}
                                                disabled={isSubmitting}
                                                className={`w-full py-2.5 rounded-xl text-[10px] lg:text-[12px] font-bold uppercase tracking-widest transition-all ${isSubmitting ? 'opacity-50 cursor-not-allowed bg-sky-900 text-white' : 'bg-sky-900 hover:bg-sky-800 text-white shadow-lg shadow-sky-800/30'}`}
                                            >
                                                {isSubmitting ? t('ahp.submitting') : t('ahp.submit_button')}
                                            </button>
                                        </div>
                                    )}
                                </div>
                            )}
                        </div>
                    )}
                </div>

                {/* Footer Controls */}
                <div className={`flex items-center justify-between p-5 lg:p-6 border-t shrink-0 ${isDarkMode ? 'bg-neutral-900 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                    {!isComplete ? (
                        <>
                            <button
                                onClick={() => setCurrentStep(Math.max(0, currentStep - 1))}
                                disabled={currentStep === 0}
                                className={`flex items-center gap-2 px-4 py-2 rounded-xl text-[10px] lg:text-[12px] font-bold uppercase tracking-widest transition-all ${currentStep === 0 ? 'opacity-30 cursor-not-allowed' : (isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-200')}`}
                            >
                                <ChevronLeft className="w-4 h-4" /> {t('common.previous')}
                            </button>

                            <button
                                onClick={() => setCurrentStep(currentStep + 1)}
                                className="flex items-center gap-2 px-6 py-2 rounded-xl text-[10px] lg:text-[12px] font-bold uppercase tracking-widest bg-sky-900 hover:bg-sky-800 text-white shadow-lg shadow-sky-800/30 transition-all"
                            >
                                {currentStep === pairs.length - 1 ? t('common.calculate') : t('common.next')} <ChevronRight className="w-4 h-4" />
                            </button>
                        </>
                    ) : (
                        <>
                            <button
                                onClick={() => {
                                    setCurrentStep(0);
                                    setSubmitSuccess(false);
                                    setSubmitError(false);
                                }}
                                className={`px-4 py-2 rounded-xl text-[10px] lg:text-[12px] font-bold uppercase tracking-widest transition-all ${isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-200'}`}
                            >
                                {results && results.CR > 0.1 && forceConsistency ? t('ahp.review_comparisons') : t('ahp.retake_survey')}
                            </button>
                            {(!forceConsistency || (results && results.CR <= 0.1)) && (
                                <button
                                    onClick={handleApply}
                                    className="flex items-center gap-2 px-6 py-2 rounded-xl text-[10px] lg:text-[12px] font-bold uppercase tracking-widest bg-sky-900 hover:bg-sky-800 text-white shadow-lg shadow-sky-800/30 transition-all"
                                >
                                    {t('ahp.apply_weights')}
                                </button>
                            )}
                        </>
                    )}
                </div>
            </div>
        </div>
    );
};
