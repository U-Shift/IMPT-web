import { Tooltip } from './Tooltip';

export const DetailCard = ({ label, value, unit, color = "", isDark = true, hexColor, isFullWidth }: { label: string, value: string, unit?: string, color?: string, isDark?: boolean, hexColor?: string, isFullWidth?: boolean }) => (
    <div
        className={`${isDark ? 'bg-neutral-800/60 border-neutral-700/30' : 'bg-neutral-50 border-neutral-100 shadow-sm'} rounded-2xl border transition-all hover:border-sky-800/30 flex flex-col overflow-hidden h-full ${isFullWidth ? 'ring-1 ring-sky-800/20' : ''}`}
    >
        <div className={`${isFullWidth ? 'p-6' : 'p-4'} flex-1 flex flex-col justify-between overflow-hidden`}>
            <div className="w-full relative">
                <Tooltip content={label} isDarkMode={isDark}>
                    <span className={`${isFullWidth ? 'text-[10px]' : 'text-[9px]'} block font-black opacity-30 uppercase mb-1.5 tracking-widest truncate text-left`}>{label}</span>
                </Tooltip>
            </div>
            <div className="flex items-baseline gap-1">
                <span className={`${isFullWidth ? 'text-xl' : 'text-sm'} font-black tracking-tighter ${color || (isDark ? 'text-white' : 'text-neutral-900')}`}>
                    {value || '—'}
                </span>
                {unit && (
                    <span className={`${isFullWidth ? 'text-xs' : 'text-[10px]'} font-bold opacity-30 uppercase tracking-tight ${color || (isDark ? 'text-white' : 'text-neutral-500')}`}>
                        {unit}
                    </span>
                )}
            </div>
        </div>
        {hexColor && (
            <div style={{ height: isFullWidth ? '6px' : '4px', backgroundColor: hexColor }} />
        )}
    </div>
);
