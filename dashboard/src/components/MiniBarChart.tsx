import { useMemo } from 'react';
import { BarChart, Bar, XAxis, YAxis, Tooltip as RechartsTooltip, ResponsiveContainer, Cell, LabelList } from 'recharts';
import { MetricDef } from '../types';

export const MiniBarChart = ({ data, metric, isDark, type, onSelect }: { data: any[], metric: MetricDef, isDark: boolean, type: 'highest' | 'lowest', onSelect?: (id: string | number) => void }) => {
    const isGood = (type === 'highest' && metric.higherTheBetter) || (type === 'lowest' && !metric.higherTheBetter);
    const chartColor = isGood ? '#10b981' : '#ef4444';

    // Enhance data with a max value for the "ghost" bar that holds labels
    const maxValue = useMemo(() => Math.max(...data.map(d => d.value), 0), [data]);
    const chartData = useMemo(() => data.map(d => ({ ...d, fullSpace: maxValue })), [data, maxValue]);

    return (
        <ResponsiveContainer width="100%" height="100%">
            <BarChart data={chartData} layout="vertical" margin={{ left: 0, right: 35, top: 0, bottom: 0 }}>
                <XAxis type="number" hide />
                <YAxis dataKey="name" type="category" hide />
                <RechartsTooltip cursor={{ fill: 'rgba(99, 102, 241, 0.05)' }} wrapperStyle={{ pointerEvents: 'auto' }} content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                        const d = payload[0].payload;
                        return (
                            <div
                                className={`${isDark ? 'bg-neutral-800 border-neutral-700' : 'bg-white border-neutral-100'} p-3 rounded-xl border shadow-xl flex flex-col gap-1 z-[100] cursor-pointer`}
                                onClick={() => d?.id && onSelect?.(d.id)}
                            >
                                <div className="text-[10px] font-black opacity-30 uppercase tracking-widest">{d.group || 'District'}</div>
                                <div className="text-[11px] font-black">{d.name}</div>
                                <div className="text-[11px] font-black text-sky-800 mt-1">{metric.format(d.value)} {metric.unit || ''}</div>
                            </div>
                        );
                    }
                    return null;
                }} />
                {/* Labels anchored to the full width of the grid */}
                <Bar dataKey="fullSpace" fill="transparent" isAnimationActive={false} onClick={(d) => d?.id && onSelect?.(d.id)} style={{ cursor: 'pointer' }}>
                    <LabelList
                        dataKey="name"
                        position="insideLeft"
                        offset={0}
                        formatter={(val: any) => (typeof val === 'string' && val.length > 50) ? `${val.substring(0, 47)}...` : val}
                        style={{
                            fontSize: '9px', fontWeight: 'bold', fill: isDark ? 'white' : 'black', opacity: 0.8
                        }}
                    />
                </Bar>
                <Bar dataKey="value" radius={[0, 4, 4, 0]} barSize={12} onClick={(d) => d?.id && onSelect?.(d.id)} style={{ cursor: 'pointer' }}>
                    {data.map((_, index) => (
                        <Cell key={`cell-${index}`} fill={chartColor} fillOpacity={1 - (index * 0.08)} />
                    ))}
                </Bar>
            </BarChart>
        </ResponsiveContainer>
    );
};
