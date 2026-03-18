import React, { useState, useMemo } from 'react';
import { X, ChevronRight, ChevronLeft, Check, AlertTriangle } from 'lucide-react';

type MetricDef = {
    id: string;
    label: string;
    description: string;
};

interface AHPModalProps {
    metrics: MetricDef[];
    isOpen: boolean;
    onClose: () => void;
    onApply: (weights: Record<string, number>) => void;
    isDarkMode: boolean;
}

// Maps slider [-8, 8] to Saaty Scale (1 to 9)
// e.g., -8 -> Left is 9x more important
//        0 -> Equally important
//        8 -> Right is 9x more important
const getSaatyValue = (sliderValue: number): number => {
    if (sliderValue === 0) return 1;
    if (sliderValue < 0) return Math.abs(sliderValue) + 1;
    return 1 / (sliderValue + 1);
};

export const AHPModal: React.FC<AHPModalProps> = ({ metrics, isOpen, onClose, onApply, isDarkMode }) => {
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

        // FIX: Dynamic RI Table and check for n > 2
        let CR = 0;
        if (n > 2) {
            const CI = (lambdaMax - n) / (n - 1);

            // Standard Saaty Random Index values for n=1 to n=10
            const RI_TABLE: Record<number, number> = {
                1: 0.00, 2: 0.00, 3: 0.58, 4: 0.90, 5: 1.12,
                6: 1.24, 7: 1.32, 8: 1.41, 9: 1.45, 10: 1.49
            };

            // Fallback to 1.49 if n > 10, though AHP rarely exceeds n=9
            const RI = RI_TABLE[n] || 1.49;
            CR = CI / RI;
        }

        // Identify most inconsistent pairs
        // For each pair (i,j), we check how much a_ij deviates from (w_i / w_j)
        const inconsistencyScores = pairs.map(([i, j], idx) => {
            const userVal = getSaatyValue(selections[idx]);
            const weightRatio = weights[i] / weights[j];
            // Using a ratio of deviation for consistency
            const deviation = Math.max(userVal / weightRatio, weightRatio / userVal);
            return { index: idx, deviation, pair: [i, j] };
        }).sort((a, b) => b.deviation - a.deviation);

        return { weights, CR, inconsistentPairs: inconsistencyScores.slice(0, 2) };
    }, [currentStep, pairs, selections, metrics.length]);

    if (!isOpen) return null;

    const isComplete = currentStep === pairs.length;

    // Convert weights array to record mapping for App.tsx
    const handleApply = () => {
        if (results) {
            const weightsRecord: Record<string, number> = {};
            metrics.forEach((m, idx) => {
                weightsRecord[m.id] = results.weights[idx];
            });
            onApply(weightsRecord);
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
        if (abs === 0) return 'Equally Important';
        if (abs <= 2) return 'Slightly More Important';
        if (abs <= 4) return 'Moderately More Important';
        if (abs <= 6) return 'Strongly More Important';
        return 'Extremely More Important';
    };

    return (
        <div className="fixed inset-0 z-[99999] flex items-center justify-center p-4">
            <div className="absolute inset-0 bg-black/60 backdrop-blur-sm" onClick={onClose} />
            <div className={`relative w-full max-w-2xl overflow-hidden rounded-3xl shadow-2xl ${isDarkMode ? 'bg-neutral-900 border-neutral-800 text-neutral-100' : 'bg-white border-neutral-200 text-neutral-900'} border`}>

                {/* Header */}
                <div className={`flex items-center justify-between p-6 border-b ${isDarkMode ? 'border-neutral-800' : 'border-neutral-100'}`}>
                    <div className="flex items-center gap-4">
                        <div>
                            <h2 className="text-2xl font-black uppercase tracking-widest">Dimension Survey</h2>
                            <p className={`text-base mt-1 ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>
                                Analytic Hierarchy Process (AHP)
                            </p>
                        </div>
                        <div className="h-8 w-px bg-neutral-200 dark:bg-neutral-800 hidden md:block" />
                        <label className="flex items-center gap-2 cursor-pointer group">
                            <div
                                onClick={() => setForceConsistency(!forceConsistency)}
                                className={`w-10 h-5 rounded-full relative transition-colors ${forceConsistency ? 'bg-sky-900' : (isDarkMode ? 'bg-neutral-800' : 'bg-neutral-200')}`}
                            >
                                <div className={`absolute top-1 left-1 w-3 h-3 bg-white rounded-full transition-transform ${forceConsistency ? 'translate-x-5' : ''}`} />
                            </div>
                            <span className="text-[12px] font-bold uppercase tracking-wider text-neutral-500 group-hover:text-sky-800 transition-colors">Force Consistency</span>
                        </label>
                    </div>
                    <button onClick={onClose} className={`p-2 rounded-full transition-colors ${isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-100'}`}>
                        <X className="w-6 h-6" />
                    </button>
                </div>

                {/* Content */}
                <div className="p-8 min-h-[300px] flex flex-col justify-center">
                    {!isComplete ? (
                        <div className="space-y-12">
                            <div className="text-center space-y-2">
                                <p className={`text-[12px] font-black uppercase tracking-widest ${isDarkMode ? 'text-sky-700' : 'text-sky-900'}`}>
                                    Comparison {currentStep + 1} of {pairs.length}
                                </p>
                                <h3 className="text-lg font-medium">Which dimension is more important to you?</h3>
                            </div>

                            <div className="flex items-center justify-between gap-6">
                                <div className={`flex-1 text-right p-4 rounded-xl border-2 transition-colors ${selections[currentStep] < 0 ? 'border-sky-800 bg-sky-800/10 text-sky-800' : (isDarkMode ? 'border-neutral-800' : 'border-neutral-100')}`}>
                                    <p className="font-bold text-base uppercase tracking-wider">{metrics[pairs[currentStep][0]].label}</p>
                                    <p className="text-[12px] opacity-60 mt-1">{metrics[pairs[currentStep][0]].description}</p>
                                </div>
                                <div className={`flex-1 text-left p-4 rounded-xl border-2 transition-colors ${selections[currentStep] > 0 ? 'border-sky-800 bg-sky-800/10 text-sky-800' : (isDarkMode ? 'border-neutral-800' : 'border-neutral-100')}`}>
                                    <p className="font-bold text-base uppercase tracking-wider">{metrics[pairs[currentStep][1]].label}</p>
                                    <p className="text-[12px] opacity-60 mt-1">{metrics[pairs[currentStep][1]].description}</p>
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
                                    <span className={`inline-block px-3 py-1 rounded-full text-[12px] font-bold uppercase tracking-widest ${selections[currentStep] !== 0 ? 'bg-sky-800 text-white' : (isDarkMode ? 'bg-neutral-800 text-neutral-400' : 'bg-neutral-100 text-neutral-500')}`}>
                                        {getIntensityLabel(selections[currentStep])}
                                    </span>
                                </div>
                            </div>
                        </div>
                    ) : (
                        <div className="space-y-8 animate-in fade-in zoom-in duration-300">
                            <div className="text-center space-y-2">
                                <div className="w-16 h-16 bg-green-500/20 text-green-500 rounded-full flex items-center justify-center mx-auto mb-4">
                                    <Check className="w-8 h-8" />
                                </div>
                                <h3 className="text-2xl font-black uppercase tracking-widest">Survey Complete</h3>
                                <p className={`text-base ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>Here are your calculated dimension weights.</p>
                            </div>

                            <div className="space-y-4">
                                {metrics.map((m, idx) => {
                                    const w = results?.weights[idx] || 0;
                                    const pct = (w * 100).toFixed(1);
                                    return (
                                        <div key={m.id} className="relative">
                                            <div className="flex justify-between text-[12px] font-bold uppercase tracking-wider mb-1">
                                                <span>{m.label}</span>
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
                                        <div className="text-base">
                                            <p className="font-bold uppercase tracking-wider mb-1">
                                                {forceConsistency ? 'Mathematical Inconsistency Detected' : 'Inconsistent Answers'} (CR: {(results.CR).toFixed(2)})
                                            </p>
                                            <p className="opacity-80">
                                                {forceConsistency
                                                    ? 'Force Consistency is enabled. You must adjust your preferences to reach a Consistency Ratio below 0.10 before applying weights.'
                                                    : 'Your pairwise comparisons are mathematically inconsistent. The weights will still work, but you may want to retake the survey for better accuracy.'}
                                            </p>
                                        </div>
                                    </div>

                                    <div className={`p-3 rounded-lg ${isDarkMode ? 'bg-neutral-800/50' : 'bg-white/50'} space-y-2`}>
                                        <p className="text-[12px] font-black uppercase tracking-widest opacity-60">Most inconsistent comparisons:</p>
                                        {results.inconsistentPairs.map((item, idx) => (
                                            <div key={idx} className="flex items-center justify-between text-[12px] group">
                                                <span className="font-bold">
                                                    {metrics[item.pair[0]].label} vs {metrics[item.pair[1]].label}
                                                </span>
                                                <button
                                                    onClick={() => setCurrentStep(item.index)}
                                                    className="px-2 py-0.5 rounded bg-sky-800 text-white font-black uppercase tracking-tighter hover:bg-sky-700 transition-colors"
                                                >
                                                    Fix
                                                </button>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            )}
                        </div>
                    )}
                </div>

                {/* Footer Controls */}
                <div className={`flex items-center justify-between p-6 border-t ${isDarkMode ? 'bg-neutral-900 border-neutral-800' : 'bg-neutral-50 border-neutral-100'}`}>
                    {!isComplete ? (
                        <>
                            <button
                                onClick={() => setCurrentStep(Math.max(0, currentStep - 1))}
                                disabled={currentStep === 0}
                                className={`flex items-center gap-2 px-4 py-2 rounded-xl text-[12px] font-bold uppercase tracking-widest transition-all ${currentStep === 0 ? 'opacity-30 cursor-not-allowed' : (isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-200')}`}
                            >
                                <ChevronLeft className="w-4 h-4" /> Previous
                            </button>

                            <button
                                onClick={() => setCurrentStep(currentStep + 1)}
                                className="flex items-center gap-2 px-6 py-2 rounded-xl text-[12px] font-bold uppercase tracking-widest bg-sky-900 hover:bg-sky-800 text-white shadow-lg shadow-sky-800/30 transition-all"
                            >
                                {currentStep === pairs.length - 1 ? 'Calculate' : 'Next'} <ChevronRight className="w-4 h-4" />
                            </button>
                        </>
                    ) : (
                        <>
                            <button
                                onClick={() => { setCurrentStep(0); }}
                                className={`px-4 py-2 rounded-xl text-[12px] font-bold uppercase tracking-widest transition-all ${isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-200'}`}
                            >
                                {results && results.CR > 0.1 && forceConsistency ? 'Review Comparisons' : 'Retake Survey'}
                            </button>
                            {(!forceConsistency || (results && results.CR <= 0.1)) && (
                                <button
                                    onClick={handleApply}
                                    className="flex items-center gap-2 px-6 py-2 rounded-xl text-[12px] font-bold uppercase tracking-widest bg-sky-900 hover:bg-sky-800 text-white shadow-lg shadow-sky-800/30 transition-all"
                                >
                                    Apply Weights
                                </button>
                            )}
                        </>
                    )}
                </div>
            </div>
        </div>
    );
};
