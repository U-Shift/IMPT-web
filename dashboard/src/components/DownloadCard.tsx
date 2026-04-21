import { ViewLevel } from '../types';
import { LEVEL_CONFIG } from '../constants';

export const DownloadCard = ({ title, id, group_id, region_id, isDark, level, filename }: { title: string, id?: string, group_id?: string, region_id?: string, isDark: boolean, level: ViewLevel, filename: string }) => {
    return (
        <div className={`${isDark ? 'bg-neutral-800/40 border-neutral-700/50' : 'bg-white border-neutral-200 shadow-sm'} p-6 rounded-[32px] border flex flex-col items-center gap-4`}>
            <div className="text-center">
                <h3 className="text-sm font-black tracking-tight">{title}</h3>
                {id && <p className="text-[9px] font-bold opacity-30 mt-1 tracking-tight">id: {id}</p>}
                {group_id && <p className="text-[9px] font-bold opacity-30 mt-1 tracking-tight">group_id: {group_id}</p>}
                {region_id && <p className="text-[9px] font-bold opacity-30 mt-1 tracking-tight">region_id: {region_id}</p>}
            </div>
            <div className="flex flex-col gap-2 w-full">
                <a
                    href={LEVEL_CONFIG[level].download_geojson}
                    target="_blank"
                    rel="noopener noreferrer"
                    download={`${filename}.json`}
                    className="w-full py-2.5 rounded-xl bg-sky-900 hover:bg-sky-800 text-white text-[12px] font-black uppercase tracking-widest transition-all cursor-pointer flex items-center justify-center text-center"
                >
                    GeoJSON
                </a>
                <a
                    href={LEVEL_CONFIG[level].download_csv}
                    target="_blank"
                    rel="noopener noreferrer"
                    download={`${filename}.csv`}
                    className="w-full py-2.5 rounded-xl border border-sky-900/30 text-sky-900 hover:bg-sky-800 hover:text-white text-[12px] font-black uppercase tracking-widest transition-all cursor-pointer flex items-center justify-center text-center"
                >
                    CSV
                </a>
            </div>
        </div>
    );
};


