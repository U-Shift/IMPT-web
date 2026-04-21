import { useMemo } from 'react';
import { BarChart, Bar, XAxis, YAxis, Tooltip as RechartsTooltip, ResponsiveContainer, Cell, LabelList } from 'recharts';
import { MetricDef } from '../types';
import { ModeId } from '../constants';

export const MiniBarChart = ({ data, metric, selectedMode, isDark, type, onSelect }: { data: any[], metric: MetricDef, selectedMode?: ModeId, isDark: boolean, type?: 'highest' | 'lowest', onSelect?: (id: string | number) => void }) => {

    // Enhance data with a max value for the "ghost" bar that holds labels
    const minValue = useMemo(() => Math.min(...data.map(d => d.value), 0), [data]);
    const maxValue = useMemo(() => Math.max(...data.map(d => d.value), 0), [data]);
    const chartData = useMemo(() => data.map(d => ({ ...d, fullSpace: [minValue, maxValue] })), [data, minValue, maxValue]);

    return (
        <ResponsiveContainer width="100%" height="100%">
            <BarChart data={chartData} layout="vertical" margin={{ left: 0, right: 35, top: 0, bottom: 0 }}>
                <XAxis type="number" hide domain={[minValue, maxValue]} />
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
                                <div className="text-[13px] font-black">{d.name}</div>
                                <div className="text-[13px] font-black mt-1" style={{ color: d.color }}>{metric.format(d.value, minValue, maxValue, selectedMode || 'all')} {metric.unit?.(selectedMode || 'all', d.value) || ''}</div>
                            </div>
                        );
                    }
                    return null;
                }} />
                {/* Labels anchored to the full width of the grid */}
                <Bar
                    dataKey="fullSpace"
                    fill="rgba(0,0,0,0)"
                    isAnimationActive={false}
                    onClick={(d) => d?.id && onSelect?.(d.id)}
                    style={{ cursor: 'pointer' }}
                >
                    <LabelList
                        dataKey="name"
                        content={(props: any) => {
                            const { y, value } = props;
                            return (
                                <foreignObject x={0} y={y - 14} width="90%" height="28" style={{ pointerEvents: 'none' }}>
                                    <div style={{
                                        width: '100%',
                                        height: '100%',
                                        display: 'flex',
                                        alignItems: 'center',
                                        paddingLeft: '4px',
                                        fontSize: '12px',
                                        fontWeight: 'bold',
                                        color: isDark ? 'white' : 'black',
                                        opacity: 0.8,
                                        marginTop: '0.2rem'
                                    }}>
                                        <span style={{
                                            overflow: 'hidden',
                                            textOverflow: 'ellipsis',
                                            whiteSpace: 'nowrap',
                                            width: '100%',
                                            display: 'block'
                                        }}>
                                            {value}
                                        </span>
                                    </div>
                                </foreignObject>
                            );
                        }}
                    />
                </Bar>
                <Bar
                    dataKey="value"
                    radius={[0, 4, 4, 0]}
                    barSize={28}
                    onClick={(d) => d?.id && onSelect?.(d.id)}
                    style={{ cursor: 'pointer' }}
                >
                    {chartData.map((d, index) => (
                        <Cell key={`cell-${index}`} fill={d.color} />
                    ))}
                </Bar>
            </BarChart>
        </ResponsiveContainer>
    );
};
