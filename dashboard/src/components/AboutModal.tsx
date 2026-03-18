import React from 'react';
import { X, Github, ExternalLink } from 'lucide-react';

interface AboutModalProps {
    showAbout: boolean;
    setShowAbout: (val: boolean) => void;
    isDarkMode: boolean;
}

export const AboutModal: React.FC<AboutModalProps> = ({ showAbout, setShowAbout, isDarkMode }) => {
    if (!showAbout) return null;

    return (
        <div className="fixed inset-0 z-[5000] flex items-center justify-center bg-black/85 backdrop-blur-xl p-8" onClick={() => setShowAbout(false)}>
            <div className={`${isDarkMode ? 'bg-neutral-900 border-neutral-800 shadow-[0_0_50px_rgba(0,0,0,1)]' : 'bg-white border-neutral-200 shadow-2xl'} border rounded-[48px] max-w-2xl w-full p-14 relative transition-all animate-in zoom-in-95 duration-300`} onClick={e => e.stopPropagation()}>
                <button onClick={() => setShowAbout(false)} className="absolute top-10 right-10 p-3 hover:bg-neutral-200 rounded-full transition-colors flex items-center justify-center">
                    <X className="w-6 h-6 opacity-40 text-neutral-500" />
                </button>
                <div className="flex items-center gap-6 mb-12">
                    <img src="images/logo/icon.png" alt="IMPT Logo" className="w-16 h-16" />
                    <div>
                        <h2 className="text-3xl font-black leading-none tracking-tighter">IMPT</h2>
                        <p className="text-neutral-500 font-black text-[12px] uppercase tracking-[0.4em] mt-3">Index for Monitoring <span className="text-sky-800">Transport Poverty</span></p>
                    </div>
                </div>
                <div className="space-y-8 text-base font-bold tracking-widest leading-relaxed opacity-60">
                    <p className={`p-6 rounded-2xl ${isDarkMode ? 'bg-white/5' : 'bg-neutral-50'} border-l-4 border-sky-900 font-medium`}>
                        Transport poverty refers to an individual or household inability to access essential services, opportunities, and social participation due to limited, inadequate, unaffordable, unreliable or non-existent means of transport (Mejía Dorantes and Murauskaite-Bull, 2022). However, due to the lack of a universal definition and its multidimensional nature the identification and measurement of transport poverty can be challenging.
                        <br />
                        <br />
                        This project aims to tackle this challenge by developing a multidimensional index that enables its measurement and monitoring over time at both national and regional levels, with the aim of identifying priority areas for intervention and supporting a just mobility transition.
                    </p>
                    <div className="grid grid-cols-2 gap-4 pt-4 uppercase">
                        <a href="https://github.com/U-Shift/IMPT-data" target="_blank" rel="noreferrer" className="flex items-center gap-4 p-5 rounded-2xl border border-neutral-800 hover:bg-sky-900 hover:text-white transition-all hover:border-sky-900 hover:scale-[1.02] active:scale-95">
                            <Github className="w-5 h-5" /> <span>Source Code</span>
                        </a>
                        <a href="https://ushift.tecnico.ulisboa.pt/" target="_blank" rel="noreferrer" className="flex items-center gap-4 p-5 rounded-2xl border border-neutral-800 hover:bg-sky-900 hover:text-white transition-all hover:border-sky-900 hover:scale-[1.02] active:scale-95">
                            <ExternalLink className="w-5 h-5" /> <span>U-Shift Lab</span>
                        </a>
                    </div>
                    <div className="pt-6 flex flex-col justify-between items-center opacity-40 text-center">
                        <p className="text-[12px]">Funded by: <strong>PLANAPP</strong></p>
                        <p className="text-[12px]">&copy; U-Shift, CERIS, Instituto Superior Técnico, University of Lisbon</p>
                    </div>
                </div>
            </div>
        </div>
    );
};
