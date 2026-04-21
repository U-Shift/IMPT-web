import React from 'react';
import { X, Download } from 'lucide-react';
import { DownloadCard } from './DownloadCard';
import { useTranslation, Trans } from 'react-i18next';

interface DownloadModalProps {
    showDownload: boolean;
    setShowDownload: (val: boolean) => void;
    isDarkMode: boolean;
}

export const DownloadModal: React.FC<DownloadModalProps> = ({ showDownload, setShowDownload, isDarkMode }) => {
    const { t } = useTranslation();
    if (!showDownload) return null;

    return (
        <div className="fixed inset-0 z-[5000] lg:flex lg:items-center lg:justify-center bg-black/85 backdrop-blur-xl p-0 lg:p-8" onClick={() => setShowDownload(false)}>
            <div className={`
                ${isDarkMode ? 'bg-neutral-900' : 'bg-white'} 
                lg:border ${isDarkMode ? 'lg:border-neutral-800' : 'lg:border-neutral-200'}
                lg:rounded-[48px] lg:max-w-2xl w-full h-[100dvh] lg:h-auto lg:max-h-[90vh] relative transition-all animate-in zoom-in-95 duration-300 flex flex-col overflow-hidden
            `} onClick={e => e.stopPropagation()}>
                
                {/* Header (Fixed) */}
                <div className="p-8 lg:p-14 pb-4 lg:pb-6 shrink-0">
                    <button onClick={() => setShowDownload(false)} className="absolute top-6 right-6 lg:top-10 lg:right-10 p-3 hover:bg-neutral-200/20 rounded-full transition-colors flex items-center justify-center z-10">
                        <X className="w-6 h-6 opacity-40 text-neutral-500" />
                    </button>
                    <div className="flex items-center gap-4 lg:gap-6">
                        <div className="w-12 h-12 lg:w-16 lg:h-16 bg-sky-900 rounded-[16px] lg:rounded-[20px] flex items-center justify-center shadow-2xl shadow-emerald-500/20">
                            <Download className="text-white w-6 h-6 lg:w-9 lg:h-9" />
                        </div>
                        <div>
                            <h2 className="text-2xl lg:text-3xl font-black leading-none tracking-tighter">{t('download.title')}</h2>
                            <p className="text-neutral-500 font-black text-[10px] lg:text-[12px] uppercase tracking-[0.4em] mt-2 lg:mt-3">{t('download.subtitle')}</p>
                        </div>
                    </div>
                </div>

                {/* Content (Scrollable) */}
                <div className="p-8 lg:p-14 pt-2 lg:pt-4 overflow-y-auto flex-1">
                    <div className="space-y-6">
                        <div className="grid grid-cols-1 lg:grid-cols-3 gap-4 lg:gap-6 mb-auto">
                            <DownloadCard
                                title={t('map.freguesia')}
                                id="DRMNFR (2024)"
                                group_id="DRMN (2024)"
                                region_id="NUTS 2"
                                isDark={isDarkMode}
                                level="freguesia"
                                filename="impt_lisbon_freguesias"
                            />
                            <DownloadCard
                                title={t('map.municipality')}
                                id="DRMN (2024)"
                                region_id="NUTS 2"
                                isDark={isDarkMode}
                                level="municipality"
                                filename="impt_lisbon_municipalities"
                            />
                            <DownloadCard
                                title={t('map.grid')}
                                id="Sequential ID"
                                group_id="DRMNFR (2024)"
                                region_id="NUTS 2"
                                isDark={isDarkMode}
                                level="hex"
                                filename="impt_lisbon_grid"
                            />
                        </div>

                        <div className={`p-5 lg:p-6 rounded-3xl ${isDarkMode ? 'bg-white/5 border-neutral-800' : 'bg-neutral-50 border-neutral-100'} border mt-6 mb-4`}>
                            <h4 className="text-[10px] lg:text-[12px] font-black opacity-40 uppercase tracking-widest mb-3">{t('download.about_data')}</h4>
                            <p className="text-[12px] lg:text-[13px] leading-relaxed opacity-60 font-medium">
                                <Trans i18nKey="download.description" values={{ format1: 'GeoJSON', format2: 'CSV', crs: 'WGS84 (EPSG:4326)', attribute: 'group_id' }}>
                                    All datasets are provided in <span className="font-bold text-sky-800">GeoJSON</span> for spatial analysis and <span className="font-bold text-sky-900">CSV</span> for tabular processing. Coordinates use <span className="font-bold">WGS84 (EPSG:4326)</span>. <span className="font-bold">group_id</span> attribute should be used to relate the files.
                                </Trans>
                            </p>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
};
