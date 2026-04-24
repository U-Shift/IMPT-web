import React, { useState, useEffect } from 'react';
import { Joyride, Step, EventData, STATUS, Placement } from 'react-joyride';
import { useTranslation } from 'react-i18next';
import { Languages, PlayCircle, Sidebar } from 'lucide-react';

interface TutorialProps {
    isDarkMode: boolean;
    runExternal?: boolean;
    onSetRunExternal?: (val: boolean) => void;
    tourType?: 'main' | 'dynamic';
    isMobile?: boolean;
    isSidebarOpen?: boolean;
    setIsSidebarOpen?: (val: boolean) => void;
}

export const Tutorial: React.FC<TutorialProps> = ({
    isDarkMode,
    runExternal,
    onSetRunExternal,
    tourType = 'main',
    isMobile = false,
    isSidebarOpen,
    setIsSidebarOpen
}) => {
    const { t, i18n } = useTranslation();
    const [run, setRun] = useState(false);
    const [showPrompt, setShowPrompt] = useState(false);

    const toggleLanguage = () => {
        i18n.changeLanguage(i18n.language === 'en' ? 'pt' : 'en');
    };

    const mainSteps: Step[] = [
        {
            target: 'body',
            placement: 'center',
            content: t('tutorial.step_1'),
            sidebar: 'closed'
        } as any,
        ...(isMobile ? [{
            target: '[data-tour="mobile-menu-btn"]',
            content: t('tutorial.mobile_step_menu'),
            placement: 'right' as Placement,
            sidebar: 'closed'
        } as any] : [
            {
                target: '[data-tour="sidebar-left"]',
                content: t('tutorial.step_2'),
                placement: "right",
                sidebar: 'open'
            } as any
        ]),
        {
            target: '[data-tour="section-metrics.categories.mobility_poverty_index"]',
            content: t('tutorial.step_3'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="section-metrics.categories.dimensions"]',
            content: t('tutorial.step_4'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="tab-indicators"]',
            content: t('tutorial.step_5'),
            sidebar: 'open'
        } as any,
        {
            target: isMobile ? '[data-tour="mobile-mode-selector"]' : '[data-tour="mode-selector"]',
            content: t('tutorial.step_6'),
            placement: isMobile ? 'right' : 'bottom',
            sidebar: 'closed'
        } as any,
        {
            target: isMobile ? '[data-tour="mobile-view-level"]' : '[data-tour="view-level"]',
            content: t('tutorial.step_7'),
            placement: isMobile ? 'top' : 'bottom',
            sidebar: 'open'
        } as any,
        {
            target: isMobile ? '[data-tour="mobile-region"]' : '[data-tour="region-selector"]',
            placement: isMobile ? 'top' : 'bottom',
            content: t('tutorial.step_8'),
            sidebar: 'open'
        } as any,
        // Skip sidebar-right on mobile
        ...(!isMobile ? [
            {
                target: '[data-tour="sidebar-right"]',
                content: t('tutorial.step_9'),
            },
            {
                target: '[data-tour="sidebar-right"]',
                content: t('tutorial.step_10'),
            },
        ] : []),
        {
            target: isMobile ? '[data-tour="mobile-map-tools"]' : '[data-tour="map-tools"]',
            content: t('tutorial.step_12'),
            sidebar: 'open'
        } as any,
        {
            target: isMobile ? '[data-tour="mobile-map-legend"]' : '[data-tour="map-legend"]',
            content: t('tutorial.step_11'),
            placement: 'top',
            sidebar: 'closed'
        } as any,
        {
            target: '[data-tour="top-controls"]',
            content: t('tutorial.step_13'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="theme-toggle"]',
            content: t('tutorial.step_14'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="colorblind-toggle"]',
            content: t('tutorial.step_15'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="language-toggle"]',
            content: t('tutorial.step_16'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="download-modal-btn"]',
            content: t('tutorial.step_17'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="about-modal-btn"]',
            content: t('tutorial.step_18'),
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="tutorial-btn"]',
            content: t('tutorial.step_19'),
            sidebar: 'open'
        } as any,
        // Skip sidebar-right on mobile
        ...(isMobile ? [
            {
                target: 'body',
                content: t('tutorial.step_10_mobile'),
                sidebar: 'closed',
                placement: 'center'
            } as any,
        ] : []),
    ];

    const dynamicSteps: Step[] = [
        {
            target: '[data-tour="dynamic-weights-section"]',
            content: t('tutorial.dynamic_step_1'),
            placement: 'right',
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="dynamic-weights-note"]',
            content: t('tutorial.dynamic_step_1_note'),
            placement: 'right',
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="ahp-recommendation"]',
            content: t('tutorial.dynamic_step_2'),
            placement: 'right',
            sidebar: 'open'
        } as any,
        {
            target: '[data-tour="download-results-btn"]',
            content: t('tutorial.dynamic_step_3'),
            placement: 'right',
            sidebar: 'open'
        } as any,
    ];

    const steps = tourType === 'main' ? mainSteps : dynamicSteps;

    useEffect(() => {
        if (runExternal) {
            setRun(true);
            if (tourType === 'dynamic') {
                setShowPrompt(false);
            }
        }
    }, [runExternal, tourType]);

    useEffect(() => {
        const completed = localStorage.getItem('tutorialCompleted');
        if (!completed) {
            setShowPrompt(true);
        }
    }, []);

    const handleStart = () => {
        setShowPrompt(false);
        setRun(true);
    };

    const handleSkip = () => {
        setShowPrompt(false);
        localStorage.setItem('tutorialCompleted', 'true');
    };

    // Fix for Joyride positions when sidebar is animating
    useEffect(() => {
        if (run) {
            const timer = setTimeout(() => {
                window.dispatchEvent(new Event('resize'));
            }, 400); // Wait for sidebar transition (300ms) + buffer
            return () => clearTimeout(timer);
        }
    }, [isSidebarOpen, run]);

    const handleJoyrideCallback = (data: EventData) => {
        const { status, type, index } = data;
        console.log("Joyride callback", status, type, index);

        if (type === 'tour:start') {
            console.log("Tour started, closing sidebar");
            if (isMobile) setIsSidebarOpen?.(false);
        }

        if (isMobile && type === 'step:after') {
            if (index + 1 < steps.length) {
                const nextStep = steps[index + 1] as any;
                console.log("> Preparing next sidebar state: ", nextStep.sidebar);
                if (nextStep && nextStep.sidebar !== undefined) {
                    if (nextStep.sidebar === 'open') {
                        setIsSidebarOpen?.(true);
                    } else if (nextStep.sidebar === 'closed') {
                        setIsSidebarOpen?.(false);
                    }
                }
            }
        }
    };

    return (
        <>
            {showPrompt && (
                <div className="fixed inset-0 z-[9999] flex items-center justify-center bg-black/50 backdrop-blur-sm">
                    <div className={`p-8 rounded-[32px] max-w-sm w-full mx-4 shadow-2xl border ${isDarkMode ? 'bg-neutral-900/95 border-neutral-800 text-white' : 'bg-white/95 border-neutral-200 text-neutral-900'}`}>
                        <img src="images/logo/logo.png" alt="IMPT Logo" className="h-12 lg:h-16 mb-4" />
                        <h2 className="text-xl font-black mb-4 uppercase tracking-tight">{t('tutorial.title_welcome')}</h2>
                        <p className={`mb-8 font-medium leading-relaxed ${isDarkMode ? 'text-neutral-400' : 'text-neutral-600'}`}>{t('tutorial.prompt_tour')}</p>
                        <div className="flex gap-4">
                            <button onClick={toggleLanguage} className={`p-3 rounded-xl font-bold transition-all flex items-center gap-2 ${isDarkMode ? 'bg-neutral-800 hover:bg-neutral-700 text-neutral-300' : 'bg-neutral-100 hover:bg-neutral-200 text-neutral-600'}`}>
                                <Languages className="w-5 h-5" />
                                <span className="text-sm uppercase">{i18n.language === 'en' ? 'PT' : 'EN'}</span>
                            </button>
                            <button onClick={handleSkip} className={`flex-1 py-3 rounded-xl font-bold transition-colors ${isDarkMode ? 'bg-neutral-800 hover:bg-neutral-700 text-neutral-300' : 'bg-neutral-100 hover:bg-neutral-200 text-neutral-600'}`}>
                                {t('tutorial.btn_skip')}
                            </button>
                            <button onClick={handleStart} className="flex-1 py-3 rounded-xl font-bold bg-sky-800 hover:bg-sky-700 text-white shadow-lg shadow-sky-800/20 transition-all">
                                {t('tutorial.btn_start')}
                            </button>
                        </div>
                    </div>
                </div>
            )}
            <Joyride
                steps={steps}
                run={run}
                continuous
                onEvent={handleJoyrideCallback}
                options={{
                    zIndex: 1010,
                    primaryColor: '#075985',
                    backgroundColor: isDarkMode ? '#171717' : '#ffffff',
                    textColor: isDarkMode ? '#f5f5f5' : '#171717',
                    arrowColor: isDarkMode ? '#171717' : '#ffffff',
                    skipBeacon: true,
                    closeButtonAction: "skip"
                }}
                styles={{
                    tooltipContainer: {
                        textAlign: 'left',
                    },
                    buttonPrimary: {
                        backgroundColor: '#075985',
                        borderRadius: '8px',
                        fontFamily: 'inherit',
                        fontWeight: 'bold',
                    },
                    buttonBack: {
                        color: isDarkMode ? '#a3a3a3' : '#525252',
                        fontFamily: 'inherit',
                        fontWeight: 'bold',
                    },
                    buttonSkip: {
                        color: isDarkMode ? '#a3a3a3' : '#525252',
                        fontFamily: 'inherit',
                        fontWeight: 'bold',
                    }
                }}
                locale={{
                    last: t('common.close'),
                    next: t('common.next'),
                    skip: t('tutorial.btn_skip'),
                    back: t('common.previous'),
                }}
            />
        </>
    );
};
