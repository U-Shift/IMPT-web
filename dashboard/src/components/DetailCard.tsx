export const DetailCard = ({ label, value, color = "", isDark = true, hexColor }: { label: string, value: string, color?: string, isDark?: boolean, hexColor?: string }) => (
    <div
        className={`${isDark ? 'bg-neutral-800/60 border-neutral-700/30' : 'bg-neutral-50 border-neutral-100 shadow-sm'} rounded-2xl border transition-all hover:border-sky-800/30 flex flex-col overflow-hidden`}
    >
        <div className="p-4 flex-1 flex flex-col justify-between">
            <span className="block text-[9px] font-black opacity-30 uppercase mb-1.5 tracking-widest">{label}</span>
            <span className={`text-sm font-black tracking-tighter ${color || (isDark ? 'text-white' : 'text-neutral-900')}`}>
                {value || '—'}
            </span>
        </div>
        {hexColor && (
            <div style={{ height: '4px', backgroundColor: hexColor }} />
        )}
    </div>
);
