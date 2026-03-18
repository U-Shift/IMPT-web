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

    // Calculate AHP Weights & Consistency
    const results = useMemo(() => {
        if (currentStep !== pairs.length) return null; // Only calculate at the end
        
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

        const CI = (lambdaMax - n) / (n - 1);
        const RI = 0.90; // Standard RI for n=4
        const CR = CI / RI;

        return { weights, CR };
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
                    <div>
                        <h2 className="text-xl font-black uppercase tracking-widest">Dimension Survey</h2>
                        <p className={`text-xs mt-1 ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>
                            Analytic Hierarchy Process (AHP)
                        </p>
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
                                <p className={`text-[10px] font-black uppercase tracking-widest ${isDarkMode ? 'text-indigo-400' : 'text-indigo-600'}`}>
                                    Comparison {currentStep + 1} of {pairs.length}
                                </p>
                                <h3 className="text-lg font-medium">Which dimension is more important to you?</h3>
                            </div>

                            <div className="flex items-center justify-between gap-6">
                                <div className={`flex-1 text-right p-4 rounded-xl border-2 transition-colors ${selections[currentStep] < 0 ? 'border-indigo-500 bg-indigo-500/10 text-indigo-500' : (isDarkMode ? 'border-neutral-800' : 'border-neutral-100')}`}>
                                    <p className="font-bold text-sm uppercase tracking-wider">{metrics[pairs[currentStep][0]].label}</p>
                                    <p className="text-[10px] opacity-60 mt-1">{metrics[pairs[currentStep][0]].description}</p>
                                </div>
                                <div className={`flex-1 text-left p-4 rounded-xl border-2 transition-colors ${selections[currentStep] > 0 ? 'border-indigo-500 bg-indigo-500/10 text-indigo-500' : (isDarkMode ? 'border-neutral-800' : 'border-neutral-100')}`}>
                                    <p className="font-bold text-sm uppercase tracking-wider">{metrics[pairs[currentStep][1]].label}</p>
                                    <p className="text-[10px] opacity-60 mt-1">{metrics[pairs[currentStep][1]].description}</p>
                                </div>
                            </div>

                            <div className="space-y-6">
                                <input
                                    type="range"
                                    min="-8" max="8" step="1"
                                    value={selections[currentStep]}
                                    onChange={handleSliderChange}
                                    className={`w-full h-2 rounded-lg appearance-none cursor-pointer accent-indigo-500 outline-none ${isDarkMode ? 'bg-neutral-800' : 'bg-neutral-200'}`}
                                />
                                <div className="text-center h-6">
                                    <span className={`inline-block px-3 py-1 rounded-full text-[10px] font-bold uppercase tracking-widest ${selections[currentStep] !== 0 ? 'bg-indigo-500 text-white' : (isDarkMode ? 'bg-neutral-800 text-neutral-400' : 'bg-neutral-100 text-neutral-500')}`}>
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
                                <p className={`text-sm ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>Here are your calculated dimension weights.</p>
                            </div>

                            <div className="space-y-4">
                                {metrics.map((m, idx) => {
                                    const w = results?.weights[idx] || 0;
                                    const pct = (w * 100).toFixed(1);
                                    return (
                                        <div key={m.id} className="relative">
                                            <div className="flex justify-between text-[10px] font-bold uppercase tracking-wider mb-1">
                                                <span>{m.label}</span>
                                                <span>{pct}%</span>
                                            </div>
                                            <div className={`h-2 rounded-full overflow-hidden ${isDarkMode ? 'bg-neutral-800' : 'bg-neutral-100'}`}>
                                                <div className="h-full bg-indigo-500 transition-all duration-1000 ease-out" style={{ width: `${pct}%` }} />
                                            </div>
                                        </div>
                                    );
                                })}
                            </div>

                            {results && results.CR > 0.1 && (
                                <div className="flex items-start gap-3 p-4 bg-amber-500/10 text-amber-500 rounded-xl">
                                    <AlertTriangle className="w-5 h-5 shrink-0 mt-0.5" />
                                    <div className="text-xs">
                                        <p className="font-bold uppercase tracking-wider mb-1">Inconsistent Answers (CR: {(results.CR).toFixed(2)})</p>
                                        <p className="opacity-80">Your pairwise comparisons are mathematically inconsistent. The weights will still work, but you may want to retake the survey for more accurate reflection of your preferences.</p>
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
                                className={`flex items-center gap-2 px-4 py-2 rounded-xl text-[10px] font-bold uppercase tracking-widest transition-all ${currentStep === 0 ? 'opacity-30 cursor-not-allowed' : (isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-200')}`}
                            >
                                <ChevronLeft className="w-4 h-4" /> Previous
                            </button>
                            
                            <button
                                onClick={() => setCurrentStep(currentStep + 1)}
                                className="flex items-center gap-2 px-6 py-2 rounded-xl text-[10px] font-bold uppercase tracking-widest bg-indigo-600 hover:bg-indigo-500 text-white shadow-lg shadow-indigo-500/30 transition-all"
                            >
                                {currentStep === pairs.length - 1 ? 'Calculate' : 'Next'} <ChevronRight className="w-4 h-4" />
                            </button>
                        </>
                    ) : (
                        <>
                            <button
                                onClick={() => { setCurrentStep(0); setSelections(new Array(pairs.length).fill(0)); }}
                                className={`px-4 py-2 rounded-xl text-[10px] font-bold uppercase tracking-widest transition-all ${isDarkMode ? 'hover:bg-neutral-800' : 'hover:bg-neutral-200'}`}
                            >
                                Retake Survey
                            </button>
                            <button
                                onClick={handleApply}
                                className="flex items-center gap-2 px-6 py-2 rounded-xl text-[10px] font-bold uppercase tracking-widest bg-indigo-600 hover:bg-indigo-500 text-white shadow-lg shadow-indigo-500/30 transition-all"
                            >
                                Apply Weights
                            </button>
                        </>
                    )}
                </div>
            </div>
        </div>
    );
};
