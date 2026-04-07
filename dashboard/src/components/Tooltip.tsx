import React, { useState, useRef, useEffect } from 'react';
import { createPortal } from 'react-dom';

interface TooltipProps {
    content: string;
    children: React.ReactNode;
    isDarkMode?: boolean;
}

export const Tooltip: React.FC<TooltipProps> = ({ content, children, isDarkMode = true }) => {
    const [isVisible, setIsVisible] = useState(false);
    const [position, setPosition] = useState({ top: 0, left: 0 });
    const targetRef = useRef<HTMLDivElement>(null);

    const updatePosition = () => {
        if (targetRef.current) {
            const rect = targetRef.current.getBoundingClientRect();
            // Position above the element, centered
            setPosition({
                top: rect.top - 10, // 10px gap
                left: rect.left + rect.width / 2
            });
        }
    };

    useEffect(() => {
        if (isVisible) {
            updatePosition();
            window.addEventListener('scroll', updatePosition, true);
            window.addEventListener('resize', updatePosition);
        }
        return () => {
            window.removeEventListener('scroll', updatePosition, true);
            window.removeEventListener('resize', updatePosition);
        };
    }, [isVisible]);

    return (
        <div
            ref={targetRef}
            onMouseEnter={() => setIsVisible(true)}
            onMouseLeave={() => setIsVisible(false)}
            className="relative inline-block w-full"
        >
            {children}
            {isVisible && createPortal(
                <div
                    style={{
                        position: 'fixed',
                        top: `${position.top}px`,
                        left: `${position.left}px`,
                        transform: 'translate(-50%, -100%)',
                        zIndex: 10000,
                        pointerEvents: 'none'
                    }}
                    className={`px-4 py-2.5 rounded-xl text-[13px] font-medium leading-relaxed max-w-[260px] w-max whitespace-pre-line shadow-2xl border backdrop-blur-xl transition-all duration-200 animate-in fade-in slide-in-from-bottom-1 ${
                        isDarkMode 
                        ? 'bg-neutral-900/95 border-neutral-700/50 text-white shadow-black/40' 
                        : 'bg-white/95 border-neutral-200 text-neutral-900 shadow-neutral-200/50'
                    }`}
                >
                    {content}
                </div>,
                document.body
            )}
        </div>
    );
};
