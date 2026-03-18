import React, { useState, useRef, useEffect } from 'react';
import { ChevronDown } from 'lucide-react';

interface Option {
    id: string;
    label: string;
    icon?: string | React.ReactNode;
}

interface MapFilterDropdownProps {
    label: string;
    value: string;
    options: Option[];
    onChange: (id: string) => void;
    isDark: boolean;
    icon?: React.ReactNode;
}

export const MapFilterDropdown: React.FC<MapFilterDropdownProps> = ({
    label, value, options, onChange, isDark, icon
}) => {
    const [isOpen, setIsOpen] = useState(false);
    const dropdownRef = useRef<HTMLDivElement>(null);

    const selectedOption = options.find(opt => opt.id === value) || options[0];

    useEffect(() => {
        const handleClickOutside = (event: MouseEvent) => {
            if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
                setIsOpen(false);
            }
        };
        document.addEventListener('mousedown', handleClickOutside);
        return () => document.removeEventListener('mousedown', handleClickOutside);
    }, []);

    return (
        <div className="relative pointer-events-auto" ref={dropdownRef}>
            <button
                onClick={() => setIsOpen(!isOpen)}
                className={`
                    flex items-center gap-3 px-4 py-2.5 rounded-2xl border transition-all duration-300
                    backdrop-blur-md shadow-xl hover:scale-[1.02] active:scale-95
                    ${isDark 
                        ? 'bg-neutral-900/90 border-neutral-800 text-neutral-300 hover:text-white' 
                        : 'bg-white/90 border-neutral-200 text-neutral-600 hover:text-neutral-900'}
                `}
            >
                {icon && <span className="opacity-50">{icon}</span>}
                <div className="text-left">
                    <p className="text-[10px] font-black uppercase tracking-[0.2em] opacity-40 leading-none mb-1">{label}</p>
                    <div className="flex items-center gap-2">
                        {selectedOption.icon && <span className="text-base leading-none">{selectedOption.icon}</span>}
                        <span className="text-[13px] font-black uppercase tracking-widest">{selectedOption.label}</span>
                    </div>
                </div>
                <ChevronDown className={`w-4 h-4 transition-transform duration-300 opacity-40 ${isOpen ? 'rotate-180' : ''}`} />
            </button>

            {isOpen && (
                <div 
                    className={`
                        absolute top-full left-0 mt-2 w-max min-w-[200px] z-[2000] 
                        rounded-2xl border overflow-hidden shadow-2xl animate-in fade-in slide-in-from-top-2 duration-200
                        backdrop-blur-xl
                        ${isDark ? 'bg-neutral-900/95 border-neutral-800' : 'bg-white/95 border-neutral-100'}
                    `}
                >
                    <div className="p-1.5 flex flex-col">
                        {options.map((option) => (
                            <button
                                key={option.id}
                                onClick={() => {
                                    onChange(option.id);
                                    setIsOpen(false);
                                }}
                                className={`
                                    flex items-center gap-3 px-4 py-3 rounded-xl text-[12px] font-black uppercase tracking-widest transition-all
                                    ${value === option.id 
                                        ? 'bg-sky-900 text-white shadow-lg' 
                                        : `${isDark ? 'text-neutral-500 hover:bg-white/5 hover:text-neutral-200' : 'text-neutral-400 hover:bg-neutral-50 hover:text-neutral-800'}`}
                                `}
                            >
                                {option.icon && <span className="text-base">{option.icon}</span>}
                                {option.label}
                            </button>
                        ))}
                    </div>
                </div>
            )}
        </div>
    );
};
