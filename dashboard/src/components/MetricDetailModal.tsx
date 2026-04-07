import React from 'react';
import { createPortal } from 'react-dom';
import { X, Info, Database } from 'lucide-react';
import { useTranslation } from 'react-i18next';
import { MetricDef } from '../types';

interface MetricDetailModalProps {
    metric: MetricDef | null;
    onClose: () => void;
    isDarkMode: boolean;
}

export const MetricDetailModal: React.FC<MetricDetailModalProps> = ({ metric, onClose, isDarkMode }) => {
    const { t } = useTranslation();
    if (!metric) return null;

    return createPortal(
        <div className="fixed inset-0 z-[5001] flex items-center justify-center bg-black/40 backdrop-blur-md p-6" onClick={onClose}>
            <div
                className={`${isDarkMode ? 'bg-neutral-900 border-neutral-800 text-white shadow-[0_0_50px_rgba(0,0,0,0.5)]' : 'bg-white border-neutral-200 text-neutral-900 shadow-2xl'} border rounded-[32px] max-w-lg w-full p-10 relative animate-in zoom-in-95 duration-200`}
                onClick={e => e.stopPropagation()}
            >
                <button
                    onClick={onClose}
                    className={`absolute top-6 right-6 p-2 rounded-full transition-colors ${isDarkMode ? 'hover:bg-neutral-800 text-neutral-500' : 'hover:bg-neutral-100 text-neutral-400'}`}
                >
                    <X className="w-5 h-5" />
                </button>

                <div className="flex items-center gap-4 mb-8">
                    <div className={`p-3 rounded-2xl ${isDarkMode ? 'bg-sky-500/10 text-sky-400' : 'bg-sky-50 text-sky-600'}`}>
                        <span className="text-2xl">{metric.icon}</span>
                    </div>
                    <div>
                        <h2 className="text-xl font-black tracking-tight uppercase leading-none">{t(metric.label)}</h2>
                        <p className={`text-[10px] font-bold uppercase tracking-[0.3em] mt-2 ${isDarkMode ? 'opacity-40' : 'opacity-50'}`}>{t('sidebar.metric_details')}</p>
                    </div>
                </div>

                <div className="space-y-8">
                    <div className="space-y-3">
                        <div className="flex items-center gap-2 text-[10px] font-black uppercase tracking-widest opacity-30">
                            <Info className="w-3 h-3" />
                            <span>{t('common.description')}</span>
                        </div>
                        <p className={`text-sm font-bold leading-relaxed tracking-tight ${isDarkMode ? 'text-neutral-400' : 'text-neutral-500'}`}>
                            {metric.description ? t(metric.description) : t('common.no_description')}
                        </p>
                    </div>

                    {metric.sources && metric.sources.length > 0 && (
                        <div className="space-y-4">
                            <div className="flex items-center gap-2 text-[10px] font-black uppercase tracking-widest opacity-30">
                                <Database className="w-3 h-3" />
                                <span>{t('common.sources')}</span>
                            </div>
                            <div className="flex flex-wrap gap-2">
                                {metric.sources.map(s => (
                                    <div
                                        key={s}
                                        className={`px-3 py-1.5 rounded-xl border text-[11px] font-black uppercase tracking-tighter ${isDarkMode
                                                ? 'bg-white/5 border-white/10 text-neutral-400'
                                                : 'bg-neutral-50 border-neutral-100 text-neutral-500'
                                            }`}
                                    >
                                        {t(`sources.${s}`)}
                                    </div>
                                ))}
                            </div>
                        </div>
                    )}
                </div>
            </div>
        </div>,
        document.body
    );
};
