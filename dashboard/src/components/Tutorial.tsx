import React, { useState, useEffect } from 'react';
import { Joyride, Step, EventData, STATUS } from 'react-joyride';
import { useTranslation } from 'react-i18next';

interface TutorialProps {
    isDarkMode: boolean;
}

export const Tutorial: React.FC<TutorialProps> = ({ isDarkMode }) => {
    const { t } = useTranslation();
    const [run, setRun] = useState(false);
    const [showPrompt, setShowPrompt] = useState(false);

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

    const handleJoyrideCallback = (data: EventData) => {
        const { status } = data;
        const finishedStatuses: string[] = [STATUS.FINISHED, STATUS.SKIPPED];

        if (finishedStatuses.includes(status)) {
            setRun(false);
            localStorage.setItem('tutorialCompleted', 'true');
        }
    };

    const steps: Step[] = [
        {
            target: 'body',
            placement: 'center',
            content: t('tutorial.step_1'),
        },
        {
            target: '[data-tour="sidebar-left"]',
            content: t('tutorial.step_2'),
        },
        {
            target: '[data-tour="section-metrics.categories.mobility_poverty_index"]',
            content: t('tutorial.step_3'),
        },
        {
            target: '[data-tour="section-metrics.categories.dimensions"]',
            content: t('tutorial.step_4'),
        },
        {
            target: '[data-tour="tab-indicators"]',
            content: t('tutorial.step_5'),
        },
        {
            target: '[data-tour="mode-selector"]',
            content: t('tutorial.step_6'),
        },
        {
            target: '[data-tour="view-level"]',
            content: t('tutorial.step_7'),
        },
        {
            target: '[data-tour="region-selector"]',
            content: t('tutorial.step_8'),
        },
        {
            target: '[data-tour="sidebar-right"]',
            content: t('tutorial.step_9'),
        },
        {
            target: '[data-tour="sidebar-right"]',
            content: t('tutorial.step_10'),
        },
        {
            target: '[data-tour="map-legend"]',
            content: t('tutorial.step_11'),
        },
        {
            target: '[data-tour="map-tools"]',
            content: t('tutorial.step_12'),
        },
        {
            target: '[data-tour="top-controls"]',
            content: t('tutorial.step_13'),
        },
        {
            target: '[data-tour="theme-toggle"]',
            content: t('tutorial.step_14'),
        },
        {
            target: '[data-tour="colorblind-toggle"]',
            content: t('tutorial.step_15'),
        },
        {
            target: '[data-tour="language-toggle"]',
            content: t('tutorial.step_16'),
        }
    ];

    return (
        <>
            {showPrompt && (
                <div className="fixed inset-0 z-[9999] flex items-center justify-center bg-black/50 backdrop-blur-sm">
                    <div className={`p-8 rounded-[32px] max-w-sm w-full mx-4 shadow-2xl border ${isDarkMode ? 'bg-neutral-900/95 border-neutral-800 text-white' : 'bg-white/95 border-neutral-200 text-neutral-900'}`}>
                        <h2 className="text-xl font-black mb-4 uppercase tracking-tight">{t('tutorial.title_welcome')}</h2>
                        <p className={`mb-8 font-medium leading-relaxed ${isDarkMode ? 'text-neutral-400' : 'text-neutral-600'}`}>{t('tutorial.prompt_tour')}</p>
                        <div className="flex gap-4">
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
                showProgress
                showSkipButton
                callback={handleJoyrideCallback}
                styles={{
                    options: {
                        zIndex: 10000,
                        primaryColor: '#075985',
                        backgroundColor: isDarkMode ? '#171717' : '#ffffff',
                        textColor: isDarkMode ? '#f5f5f5' : '#171717',
                        arrowColor: isDarkMode ? '#171717' : '#ffffff',
                    },
                    tooltipContainer: {
                        textAlign: 'left',
                    },
                    buttonNext: {
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
