export const DownloadCard = ({ title, id, isDark, data, filename }: { title: string, id: string, isDark: boolean, data: any, filename: string }) => {
    const downloadCSV = () => {
        if (!data || !data.features) return;
        const properties = data.features.map((f: any) => f.properties);
        if (properties.length === 0) return;

        const keys = Object.keys(properties[0]);
        const csvContent = [
            keys.join(','),
            ...properties.map((row: any) => keys.map(k => {
                const val = row[k];
                return typeof val === 'string' ? `"${val.replace(/"/g, '""')}"` : val;
            }).join(','))
        ].join('\n');

        const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
        const link = document.createElement("a");
        link.href = URL.createObjectURL(blob);
        link.setAttribute("download", `${filename}.csv`);
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    };

    const downloadGeoJSON = () => {
        if (!data) return;
        const blob = new Blob([JSON.stringify(data)], { type: 'application/json' });
        const link = document.createElement("a");
        link.href = URL.createObjectURL(blob);
        link.setAttribute("download", `${filename}.json`);
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    };

    return (
        <div className={`${isDark ? 'bg-neutral-800/40 border-neutral-700/50' : 'bg-white border-neutral-200 shadow-sm'} p-6 rounded-[32px] border flex flex-col items-center gap-4`}>
            <div className="text-center">
                <h3 className="text-sm font-black tracking-tight">{title}</h3>
                <p className="text-[9px] font-bold opacity-30 mt-1 uppercase tracking-tighter">ID: {id}</p>
            </div>
            <div className="flex flex-col gap-2 w-full">
                <button onClick={downloadGeoJSON} className="w-full py-2.5 rounded-xl bg-sky-900 hover:bg-sky-800 text-white text-[10px] font-black uppercase tracking-widest transition-all">GeoJSON</button>
                <button onClick={downloadCSV} className="w-full py-2.5 rounded-xl border border-emerald-600/30 text-emerald-500 hover:bg-emerald-600 hover:text-white text-[10px] font-black uppercase tracking-widest transition-all">CSV</button>
            </div>
        </div>
    );
};
