import React from 'react';
import { useMap } from 'react-leaflet';
import { Plus, Minus, Layers } from 'lucide-react';
import { useTranslation } from 'react-i18next';
import { MapFilterDropdown } from './MapFilterDropdown';
import { MAP_LAYERS } from '../constants';

interface MapToolsProps {
    isDarkMode: boolean;
    mapStyle: string;
    setMapStyle: (style: string) => void;
    showBuiltArea: boolean;
    setShowBuiltArea: (val: boolean) => void;
    onZoomIn?: () => void;
    onZoomOut?: () => void;
    isMobile?: boolean;
    onChange?: () => void;
}

export const MapToolsUI: React.FC<MapToolsProps> = ({
    isDarkMode, mapStyle, setMapStyle, showBuiltArea, setShowBuiltArea,
    onZoomIn, onZoomOut, isMobile, onChange
}) => {
    const { t } = useTranslation();

    const layerOptions = MAP_LAYERS.map(l => ({
        id: l.id,
        label: t(l.label),
        icon: l.icon
    }));

    if (isMobile) {
        return (
            <div className="flex flex-col gap-4">
                <div className="w-full">
                    <MapFilterDropdown
                        isMobile={true}
                        label={t('map.layers')}
                        value={mapStyle}
                        isDark={isDarkMode}
                        icon={<Layers className="w-3.5 h-3.5" />}
                        options={layerOptions}
                        onChange={(id) => {
                            setMapStyle(id);
                            onChange?.();
                        }}
                    />
                </div>

                <button
                    onClick={() => {
                        setShowBuiltArea(!showBuiltArea);
                        onChange?.();
                    }}
                    className={`w-full flex items-center justify-between px-4 py-3 rounded-2xl border transition-all ${isDarkMode ? 'bg-neutral-900/50 border-neutral-800' : 'bg-white/50 border-neutral-200'}`}
                >
                    <div className="flex items-center gap-3">
                        <div className={`p-1.5 rounded-lg ${isDarkMode ? 'bg-neutral-800 text-sky-400' : 'bg-neutral-100 text-sky-700'}`}>
                            <Layers className="w-3.5 h-3.5" />
                        </div>
                        <span className={`text-[12px] font-black uppercase tracking-widest ${isDarkMode ? 'text-neutral-300' : 'text-neutral-700'}`}>
                            {t('metrics.categories.cos_builtarea_title')}
                        </span>
                    </div>
                    <div className={`w-10 h-5 rounded-full relative transition-colors duration-300 ${showBuiltArea ? 'bg-sky-800' : (isDarkMode ? 'bg-neutral-700' : 'bg-neutral-300')}`}>
                        <div className={`absolute top-0.5 left-0.5 w-4 h-4 rounded-full bg-white transition-transform duration-300 ${showBuiltArea ? 'translate-x-5 shadow-[-2px_0_4_px_rgba(0,0,0,0.2)]' : 'translate-x-0 shadow-[2px_0_4px_rgba(0,0,0,0.1)]'}`} />
                    </div>
                </button>
            </div>
        );
    }

    return (
        <div data-tour="map-tools" className="absolute bottom-8 left-8 z-[1100] flex flex-row items-center gap-2">
            <div className="flex flex-row gap-2">
                <button
                    onClick={onZoomIn}
                    title={t('map.zoom_in')}
                    className={`
                        w-10 h-10 rounded-full flex items-center justify-center transition-all duration-300
                        backdrop-blur-md shadow-xl hover:scale-110 active:scale-90 pointer-events-auto
                        ${isDarkMode ? 'bg-neutral-900/90 border border-neutral-800 text-white' : 'bg-white/90 border border-neutral-200 text-neutral-800'}
                    `}
                >
                    <Plus className="w-5 h-5" />
                </button>
                <button
                    onClick={onZoomOut}
                    title={t('map.zoom_out')}
                    className={`
                        w-10 h-10 rounded-full flex items-center justify-center transition-all duration-300
                        backdrop-blur-md shadow-xl hover:scale-110 active:scale-90 pointer-events-auto
                        ${isDarkMode ? 'bg-neutral-900/90 border border-neutral-800 text-white' : 'bg-white/90 border border-neutral-200 text-neutral-800'}
                    `}
                >
                    <Minus className="w-5 h-5" />
                </button>
            </div>

            <MapFilterDropdown
                label={t('map.layers')}
                direction="up"
                value={mapStyle}
                isDark={isDarkMode}
                icon={<Layers className="w-3.5 h-3.5" />}
                options={layerOptions}
                onChange={(id) => {
                    setMapStyle(id);
                    onChange?.();
                }}
            />

            <div className={`flex items-center gap-3 px-4 py-2.5 rounded-2xl border transition-all duration-300 backdrop-blur-md shadow-xl cursor-pointer pointer-events-auto hover:scale-[1.02] active:scale-95 ${isDarkMode ? 'bg-neutral-900/90 border-neutral-800' : 'bg-white/90 border-neutral-200'}`}
                onClick={() => {
                    setShowBuiltArea(!showBuiltArea);
                    onChange?.();
                }}
            >
                <div className={`p-1.5 rounded-lg ${isDarkMode ? 'bg-neutral-800 text-sky-400' : 'bg-neutral-100 text-sky-700'}`}>
                    <Layers className="w-4 h-4" />
                </div>
                <div className="flex flex-col text-left">
                    <span className={`text-[12px] font-black uppercase tracking-widest ${isDarkMode ? 'text-neutral-300' : 'text-neutral-700'} leading-none`}>
                        {t('metrics.categories.cos_builtarea_title', 'Classificação de edificado')}
                    </span>
                </div>
                <button
                    className={`w-10 h-5 rounded-full relative transition-colors duration-300 ml-2 ${showBuiltArea ? 'bg-sky-800' : (isDarkMode ? 'bg-neutral-700' : 'bg-neutral-300')}`}
                >
                    <div className={`absolute top-0.5 left-0.5 w-4 h-4 rounded-full bg-white transition-transform duration-300 ${showBuiltArea ? 'translate-x-5 shadow-[-2px_0_4_px_rgba(0,0,0,0.2)]' : 'translate-x-0 shadow-[2px_0_4px_rgba(0,0,0,0.1)]'}`} />
                </button>
            </div>
        </div>
    );
};

export const MapTools: React.FC<MapToolsProps> = (props) => {
    if (props.onZoomIn && props.onZoomOut) {
        return <MapToolsUI {...props} />;
    }
    return <MapToolsWithInternalMap {...props} />;
};

const MapToolsWithInternalMap: React.FC<MapToolsProps> = (props) => {
    const map = useMap();
    const onZoomIn = () => map.setZoom(map.getZoom() + 1);
    const onZoomOut = () => map.setZoom(map.getZoom() - 1);
    return <MapToolsUI {...props} onZoomIn={onZoomIn} onZoomOut={onZoomOut} />;
};
