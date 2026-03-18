import React from 'react';
import { X, Download } from 'lucide-react';
import { DownloadCard } from './DownloadCard';

interface DownloadModalProps {
    showDownload: boolean;
    setShowDownload: (val: boolean) => void;
    isDarkMode: boolean;
    dataState: any;
}

export const DownloadModal: React.FC<DownloadModalProps> = ({ showDownload, setShowDownload, isDarkMode, dataState }) => {
    if (!showDownload) return null;

    return (
        <div className="fixed inset-0 z-[5000] flex items-center justify-center bg-black/85 backdrop-blur-xl p-8" onClick={() => setShowDownload(false)}>
            <div className={`${isDarkMode ? 'bg-neutral-900 border-neutral-800 shadow-[0_0_50px_rgba(0,0,0,1)]' : 'bg-white border-neutral-200 shadow-2xl'} border rounded-[48px] max-w-2xl w-full p-14 relative transition-all animate-in zoom-in-95 duration-300`} onClick={e => e.stopPropagation()}>
                <button onClick={() => setShowDownload(false)} className="absolute top-10 right-10 p-3 hover:bg-neutral-200 rounded-full transition-colors flex items-center justify-center">
                    <X className="w-6 h-6 opacity-40 text-neutral-500" />
                </button>
                <div className="flex items-center gap-6 mb-12">
                    <div className="w-16 h-16 bg-sky-900 rounded-[20px] flex items-center justify-center shadow-2xl shadow-emerald-500/20">
                        <Download className="text-white w-9 h-9" />
                    </div>
                    <div>
                        <h2 className="text-3xl font-black leading-none tracking-tighter">Download Data</h2>
                        <p className="text-neutral-500 font-black text-[12px] uppercase tracking-[0.4em] mt-3">Get the raw data for further analysis</p>
                    </div>
                </div>

                <div className="space-y-6">
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-auto">
                        <DownloadCard
                            title="Freguesias"
                            id="DRMNFR (2024)"
                            group_id="DRMN (2024)"
                            region_id="NUTS 2"
                            isDark={isDarkMode}
                            data={dataState.geo['freguesia']}
                            filename="impt_lisbon_freguesias"
                        />
                        <DownloadCard
                            title="Municipality"
                            id="DRMN (2024)"
                            region_id="NUTS 2"
                            isDark={isDarkMode}
                            data={dataState.geo['municipality']}
                            filename="impt_lisbon_municipalities"
                        />
                        <DownloadCard
                            title="Grid (Hex)"
                            id="Sequential ID"
                            group_id="DRMNFR (2024)"
                            region_id="NUTS 2"
                            isDark={isDarkMode}
                            data={dataState.geo['hex']}
                            filename="impt_lisbon_grid"
                        />
                    </div>

                    <div className={`p-6 rounded-3xl ${isDarkMode ? 'bg-white/5 border-neutral-800' : 'bg-neutral-50 border-neutral-100'} border mt-6`}>
                        <h4 className="text-[12px] font-black opacity-40 uppercase tracking-widest mb-3">About the data</h4>
                        <p className="text-[13px] leading-relaxed opacity-60 font-medium">All datasets are provided in <span className="font-bold text-sky-800">GeoJSON</span> for spatial analysis and <span className="font-bold text-sky-900">CSV</span> for tabular processing. Coordinates use <span className="font-bold">WGS84 (EPSG:4326)</span>. <span className="font-bold">group_id</span> attribute should be used to relate the files.</p>
                    </div>
                </div>
            </div>
        </div>
    );
};
