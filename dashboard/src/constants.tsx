import React from 'react';
import { MetricDef, ViewLevel, ScaleMethod } from './types';

const DATA_BASE_URL = import.meta.env.DEV ? 'data/' : 'https://ushift.tecnico.ulisboa.pt/content/impt/';

export const LEVEL_CONFIG: Record<ViewLevel, { file: string, parent?: ViewLevel }> = {
    'municipality': { file: `${DATA_BASE_URL}municipios_aggregated.geojson` },
    'freguesia': { file: `${DATA_BASE_URL}freguesias_aggregated.geojson`, parent: 'municipality' },
    'hex': { file: `${DATA_BASE_URL}grid_aggregated.geojson`, parent: 'freguesia' }
};

export const COLORS = {
    // For more color ramps, visit https://observablehq.com/@d3/color-schemes
    // Low values red, high values green
    RedToGreen: ["#a50026", "#a70226", "#a90426", "#ab0626", "#ad0826", "#af0926", "#b10b26", "#b30d26", "#b50f26", "#b61127", "#b81327", "#ba1527", "#bc1727", "#be1927", "#c01b27", "#c21d28", "#c41f28", "#c52128", "#c72328", "#c92529", "#cb2729", "#cc2929", "#ce2b2a", "#d02d2a", "#d12f2b", "#d3312b", "#d4332c", "#d6352c", "#d7382d", "#d93a2e", "#da3c2e", "#dc3e2f", "#dd4030", "#de4331", "#e04532", "#e14733", "#e24a33", "#e34c34", "#e44e35", "#e55136", "#e75337", "#e85538", "#e95839", "#ea5a3a", "#eb5d3c", "#ec5f3d", "#ed613e", "#ed643f", "#ee6640", "#ef6941", "#f06b42", "#f16e43", "#f17044", "#f27346", "#f37547", "#f37848", "#f47a49", "#f57d4a", "#f57f4b", "#f6824d", "#f6844e", "#f7864f", "#f78950", "#f88b51", "#f88e53", "#f89054", "#f99355", "#f99556", "#f99858", "#fa9a59", "#fa9c5a", "#fa9f5b", "#fba15d", "#fba35e", "#fba660", "#fba861", "#fcaa62", "#fcad64", "#fcaf65", "#fcb167", "#fcb368", "#fcb56a", "#fdb86b", "#fdba6d", "#fdbc6e", "#fdbe70", "#fdc071", "#fdc273", "#fdc474", "#fdc676", "#fdc878", "#fdca79", "#fecc7b", "#fecd7d", "#fecf7e", "#fed180", "#fed382", "#fed584", "#fed685", "#fed887", "#feda89", "#fedb8b", "#fedd8d", "#fede8f", "#fee090", "#fee192", "#fee394", "#fee496", "#fee698", "#fee79a", "#fee89b", "#feea9d", "#feeb9f", "#feeca0", "#feeda2", "#feeea3", "#fdefa5", "#fdf0a6", "#fdf1a7", "#fdf2a9", "#fcf3aa", "#fcf4ab", "#fcf5ab", "#fbf5ac", "#fbf6ad", "#faf6ad", "#faf7ad", "#f9f7ae", "#f8f7ae", "#f7f8ad", "#f7f8ad", "#f6f8ad", "#f5f8ac", "#f4f8ab", "#f3f8ab", "#f1f8aa", "#f0f7a9", "#eff7a8", "#eef7a6", "#edf6a5", "#ebf6a4", "#eaf6a2", "#e8f5a1", "#e7f59f", "#e6f49d", "#e4f39c", "#e2f39a", "#e1f298", "#dff297", "#def195", "#dcf093", "#daef92", "#d9ef90", "#d7ee8e", "#d5ed8d", "#d3ec8b", "#d2ec89", "#d0eb88", "#ceea86", "#cce985", "#cae983", "#c8e882", "#c6e780", "#c4e67f", "#c2e57e", "#c0e47c", "#bee47b", "#bce37a", "#bae279", "#b8e178", "#b6e076", "#b4df75", "#b2de74", "#b0dd73", "#aedc72", "#acdb71", "#a9da70", "#a7d970", "#a5d86f", "#a3d86e", "#a0d76d", "#9ed66c", "#9cd56c", "#99d36b", "#97d26b", "#95d16a", "#92d069", "#90cf69", "#8ece68", "#8bcd68", "#89cc67", "#86cb67", "#84ca66", "#81c966", "#7fc866", "#7cc665", "#79c565", "#77c464", "#74c364", "#71c263", "#6fc063", "#6cbf62", "#69be62", "#67bd62", "#64bc61", "#61ba60", "#5eb960", "#5cb85f", "#59b65f", "#56b55e", "#53b45e", "#51b25d", "#4eb15c", "#4baf5c", "#48ae5b", "#46ad5a", "#43ab5a", "#40aa59", "#3da858", "#3ba757", "#38a557", "#36a456", "#33a255", "#31a154", "#2e9f54", "#2c9d53", "#2a9c52", "#289a51", "#259950", "#23974f", "#21954f", "#1f944e", "#1e924d", "#1c904c", "#1a8f4b", "#188d4a", "#178b49", "#158948", "#148747", "#128646", "#118446", "#108245", "#0e8044", "#0d7e43", "#0c7d42", "#0b7b41", "#0a7940", "#08773f", "#07753e", "#06733d", "#05713c", "#04703b", "#036e3a", "#026c39", "#016a38", "#006837"],
    // Low values green, high values red
    GreenToRed: ["#a50026", "#a70226", "#a90426", "#ab0626", "#ad0826", "#af0926", "#b10b26", "#b30d26", "#b50f26", "#b61127", "#b81327", "#ba1527", "#bc1727", "#be1927", "#c01b27", "#c21d28", "#c41f28", "#c52128", "#c72328", "#c92529", "#cb2729", "#cc2929", "#ce2b2a", "#d02d2a", "#d12f2b", "#d3312b", "#d4332c", "#d6352c", "#d7382d", "#d93a2e", "#da3c2e", "#dc3e2f", "#dd4030", "#de4331", "#e04532", "#e14733", "#e24a33", "#e34c34", "#e44e35", "#e55136", "#e75337", "#e85538", "#e95839", "#ea5a3a", "#eb5d3c", "#ec5f3d", "#ed613e", "#ed643f", "#ee6640", "#ef6941", "#f06b42", "#f16e43", "#f17044", "#f27346", "#f37547", "#f37848", "#f47a49", "#f57d4a", "#f57f4b", "#f6824d", "#f6844e", "#f7864f", "#f78950", "#f88b51", "#f88e53", "#f89054", "#f99355", "#f99556", "#f99858", "#fa9a59", "#fa9c5a", "#fa9f5b", "#fba15d", "#fba35e", "#fba660", "#fba861", "#fcaa62", "#fcad64", "#fcaf65", "#fcb167", "#fcb368", "#fcb56a", "#fdb86b", "#fdba6d", "#fdbc6e", "#fdbe70", "#fdc071", "#fdc273", "#fdc474", "#fdc676", "#fdc878", "#fdca79", "#fecc7b", "#fecd7d", "#fecf7e", "#fed180", "#fed382", "#fed584", "#fed685", "#fed887", "#feda89", "#fedb8b", "#fedd8d", "#fede8f", "#fee090", "#fee192", "#fee394", "#fee496", "#fee698", "#fee79a", "#fee89b", "#feea9d", "#feeb9f", "#feeca0", "#feeda2", "#feeea3", "#fdefa5", "#fdf0a6", "#fdf1a7", "#fdf2a9", "#fcf3aa", "#fcf4ab", "#fcf5ab", "#fbf5ac", "#fbf6ad", "#faf6ad", "#faf7ad", "#f9f7ae", "#f8f7ae", "#f7f8ad", "#f7f8ad", "#f6f8ad", "#f5f8ac", "#f4f8ab", "#f3f8ab", "#f1f8aa", "#f0f7a9", "#eff7a8", "#eef7a6", "#edf6a5", "#ebf6a4", "#eaf6a2", "#e8f5a1", "#e7f59f", "#e6f49d", "#e4f39c", "#e2f39a", "#e1f298", "#dff297", "#def195", "#dcf093", "#daef92", "#d9ef90", "#d7ee8e", "#d5ed8d", "#d3ec8b", "#d2ec89", "#d0eb88", "#ceea86", "#cce985", "#cae983", "#c8e882", "#c6e780", "#c4e67f", "#c2e57e", "#c0e47c", "#bee47b", "#bce37a", "#bae279", "#b8e178", "#b6e076", "#b4df75", "#b2de74", "#b0dd73", "#aedc72", "#acdb71", "#a9da70", "#a7d970", "#a5d86f", "#a3d86e", "#a0d76d", "#9ed66c", "#9cd56c", "#99d36b", "#97d26b", "#95d16a", "#92d069", "#90cf69", "#8ece68", "#8bcd68", "#89cc67", "#86cb67", "#84ca66", "#81c966", "#7fc866", "#7cc665", "#79c565", "#77c464", "#74c364", "#71c263", "#6fc063", "#6cbf62", "#69be62", "#67bd62", "#64bc61", "#61ba60", "#5eb960", "#5cb85f", "#59b65f", "#56b55e", "#53b45e", "#51b25d", "#4eb15c", "#4baf5c", "#48ae5b", "#46ad5a", "#43ab5a", "#40aa59", "#3da858", "#3ba757", "#38a557", "#36a456", "#33a255", "#31a154", "#2e9f54", "#2c9d53", "#2a9c52", "#289a51", "#259950", "#23974f", "#21954f", "#1f944e", "#1e924d", "#1c904c", "#1a8f4b", "#188d4a", "#178b49", "#158948", "#148747", "#128646", "#118446", "#108245", "#0e8044", "#0d7e43", "#0c7d42", "#0b7b41", "#0a7940", "#08773f", "#07753e", "#06733d", "#05713c", "#04703b", "#036e3a", "#026c39", "#016a38", "#006837"].reverse(),
    // Reds
    WhiteToRed: ["#fff5f0", "#fff4ef", "#fff4ee", "#fff3ed", "#fff2ec", "#fff2eb", "#fff1ea", "#fff0e9", "#fff0e8", "#ffefe7", "#ffeee6", "#ffeee6", "#ffede5", "#ffece4", "#ffece3", "#ffebe2", "#feeae1", "#fee9e0", "#fee9de", "#fee8dd", "#fee7dc", "#fee6db", "#fee6da", "#fee5d9", "#fee4d8", "#fee3d7", "#fee2d6", "#fee2d5", "#fee1d4", "#fee0d2", "#fedfd1", "#feded0", "#feddcf", "#fedccd", "#fedbcc", "#fedacb", "#fed9ca", "#fed8c8", "#fed7c7", "#fdd6c6", "#fdd5c4", "#fdd4c3", "#fdd3c1", "#fdd2c0", "#fdd1bf", "#fdd0bd", "#fdcfbc", "#fdceba", "#fdcdb9", "#fdccb7", "#fdcbb6", "#fdc9b4", "#fdc8b3", "#fdc7b2", "#fdc6b0", "#fdc5af", "#fdc4ad", "#fdc2ac", "#fdc1aa", "#fdc0a8", "#fcbfa7", "#fcbea5", "#fcbca4", "#fcbba2", "#fcbaa1", "#fcb99f", "#fcb89e", "#fcb69c", "#fcb59b", "#fcb499", "#fcb398", "#fcb196", "#fcb095", "#fcaf94", "#fcae92", "#fcac91", "#fcab8f", "#fcaa8e", "#fca98c", "#fca78b", "#fca689", "#fca588", "#fca486", "#fca285", "#fca183", "#fca082", "#fc9e81", "#fc9d7f", "#fc9c7e", "#fc9b7c", "#fc997b", "#fc987a", "#fc9778", "#fc9677", "#fc9475", "#fc9374", "#fc9273", "#fc9071", "#fc8f70", "#fc8e6f", "#fc8d6d", "#fc8b6c", "#fc8a6b", "#fc8969", "#fc8868", "#fc8667", "#fc8565", "#fc8464", "#fb8263", "#fb8162", "#fb8060", "#fb7f5f", "#fb7d5e", "#fb7c5d", "#fb7b5b", "#fb795a", "#fb7859", "#fb7758", "#fb7657", "#fb7455", "#fa7354", "#fa7253", "#fa7052", "#fa6f51", "#fa6e50", "#fa6c4e", "#f96b4d", "#f96a4c", "#f9684b", "#f9674a", "#f96549", "#f86448", "#f86347", "#f86146", "#f86045", "#f75e44", "#f75d43", "#f75c42", "#f65a41", "#f65940", "#f6573f", "#f5563e", "#f5553d", "#f4533c", "#f4523b", "#f4503a", "#f34f39", "#f34e38", "#f24c37", "#f24b37", "#f14936", "#f14835", "#f04734", "#ef4533", "#ef4433", "#ee4332", "#ed4131", "#ed4030", "#ec3f2f", "#eb3d2f", "#eb3c2e", "#ea3b2d", "#e93a2d", "#e8382c", "#e7372b", "#e6362b", "#e6352a", "#e5342a", "#e43229", "#e33128", "#e23028", "#e12f27", "#e02e27", "#df2d26", "#de2c26", "#dd2b25", "#dc2a25", "#db2924", "#da2824", "#d92723", "#d72623", "#d62522", "#d52422", "#d42321", "#d32221", "#d22121", "#d12020", "#d01f20", "#ce1f1f", "#cd1e1f", "#cc1d1f", "#cb1d1e", "#ca1c1e", "#c91b1e", "#c71b1d", "#c61a1d", "#c5191d", "#c4191c", "#c3181c", "#c2181c", "#c0171b", "#bf171b", "#be161b", "#bd161a", "#bb151a", "#ba151a", "#b91419", "#b81419", "#b61419", "#b51319", "#b41318", "#b21218", "#b11218", "#b01218", "#ae1117", "#ad1117", "#ac1117", "#aa1017", "#a91016", "#a71016", "#a60f16", "#a40f16", "#a30e15", "#a10e15", "#a00e15", "#9e0d15", "#9c0d14", "#9b0c14", "#990c14", "#970c14", "#960b13", "#940b13", "#920a13", "#900a13", "#8f0a12", "#8d0912", "#8b0912", "#890812", "#870811", "#860711", "#840711", "#820711", "#800610", "#7e0610", "#7c0510", "#7a0510", "#78040f", "#76040f", "#75030f", "#73030f", "#71020e", "#6f020e", "#6d010e", "#6b010e", "#69000d", "#67000d"],
    RedToWhite: ["#fff5f0", "#fff4ef", "#fff4ee", "#fff3ed", "#fff2ec", "#fff2eb", "#fff1ea", "#fff0e9", "#fff0e8", "#ffefe7", "#ffeee6", "#ffeee6", "#ffede5", "#ffece4", "#ffece3", "#ffebe2", "#feeae1", "#fee9e0", "#fee9de", "#fee8dd", "#fee7dc", "#fee6db", "#fee6da", "#fee5d9", "#fee4d8", "#fee3d7", "#fee2d6", "#fee2d5", "#fee1d4", "#fee0d2", "#fedfd1", "#feded0", "#feddcf", "#fedccd", "#fedbcc", "#fedacb", "#fed9ca", "#fed8c8", "#fed7c7", "#fdd6c6", "#fdd5c4", "#fdd4c3", "#fdd3c1", "#fdd2c0", "#fdd1bf", "#fdd0bd", "#fdcfbc", "#fdceba", "#fdcdb9", "#fdccb7", "#fdcbb6", "#fdc9b4", "#fdc8b3", "#fdc7b2", "#fdc6b0", "#fdc5af", "#fdc4ad", "#fdc2ac", "#fdc1aa", "#fdc0a8", "#fcbfa7", "#fcbea5", "#fcbca4", "#fcbba2", "#fcbaa1", "#fcb99f", "#fcb89e", "#fcb69c", "#fcb59b", "#fcb499", "#fcb398", "#fcb196", "#fcb095", "#fcaf94", "#fcae92", "#fcac91", "#fcab8f", "#fcaa8e", "#fca98c", "#fca78b", "#fca689", "#fca588", "#fca486", "#fca285", "#fca183", "#fca082", "#fc9e81", "#fc9d7f", "#fc9c7e", "#fc9b7c", "#fc997b", "#fc987a", "#fc9778", "#fc9677", "#fc9475", "#fc9374", "#fc9273", "#fc9071", "#fc8f70", "#fc8e6f", "#fc8d6d", "#fc8b6c", "#fc8a6b", "#fc8969", "#fc8868", "#fc8667", "#fc8565", "#fc8464", "#fb8263", "#fb8162", "#fb8060", "#fb7f5f", "#fb7d5e", "#fb7c5d", "#fb7b5b", "#fb795a", "#fb7859", "#fb7758", "#fb7657", "#fb7455", "#fa7354", "#fa7253", "#fa7052", "#fa6f51", "#fa6e50", "#fa6c4e", "#f96b4d", "#f96a4c", "#f9684b", "#f9674a", "#f96549", "#f86448", "#f86347", "#f86146", "#f86045", "#f75e44", "#f75d43", "#f75c42", "#f65a41", "#f65940", "#f6573f", "#f5563e", "#f5553d", "#f4533c", "#f4523b", "#f4503a", "#f34f39", "#f34e38", "#f24c37", "#f24b37", "#f14936", "#f14835", "#f04734", "#ef4533", "#ef4433", "#ee4332", "#ed4131", "#ed4030", "#ec3f2f", "#eb3d2f", "#eb3c2e", "#ea3b2d", "#e93a2d", "#e8382c", "#e7372b", "#e6362b", "#e6352a", "#e5342a", "#e43229", "#e33128", "#e23028", "#e12f27", "#e02e27", "#df2d26", "#de2c26", "#dd2b25", "#dc2a25", "#db2924", "#da2824", "#d92723", "#d72623", "#d62522", "#d52422", "#d42321", "#d32221", "#d22121", "#d12020", "#d01f20", "#ce1f1f", "#cd1e1f", "#cc1d1f", "#cb1d1e", "#ca1c1e", "#c91b1e", "#c71b1d", "#c61a1d", "#c5191d", "#c4191c", "#c3181c", "#c2181c", "#c0171b", "#bf171b", "#be161b", "#bd161a", "#bb151a", "#ba151a", "#b91419", "#b81419", "#b61419", "#b51319", "#b41318", "#b21218", "#b11218", "#b01218", "#ae1117", "#ad1117", "#ac1117", "#aa1017", "#a91016", "#a71016", "#a60f16", "#a40f16", "#a30e15", "#a10e15", "#a00e15", "#9e0d15", "#9c0d14", "#9b0c14", "#990c14", "#970c14", "#960b13", "#940b13", "#920a13", "#900a13", "#8f0a12", "#8d0912", "#8b0912", "#890812", "#870811", "#860711", "#840711", "#820711", "#800610", "#7e0610", "#7c0510", "#7a0510", "#78040f", "#76040f", "#75030f", "#73030f", "#71020e", "#6f020e", "#6d010e", "#6b010e", "#69000d", "#67000d"].reverse(),
    // Blues
    WhiteToBlue: ['#f7fbff', '#f6faff', '#f5fafe', '#f5f9fe', '#f4f9fe', '#f3f8fe', '#f2f8fd', '#f2f7fd', '#f1f7fd', '#f0f6fd', '#eff6fc', '#eef5fc', '#eef5fc', '#edf4fc', '#ecf4fb', '#ebf3fb', '#eaf3fb', '#eaf2fb', '#e9f2fa', '#e8f1fa', '#e7f1fa', '#e7f0fa', '#e6f0f9', '#e5eff9', '#e4eff9', '#e3eef9', '#e3eef8', '#e2edf8', '#e1edf8', '#e0ecf8', '#dfecf7', '#dfebf7', '#deebf7', '#ddeaf7', '#dceaf6', '#dce9f6', '#dbe9f6', '#dae8f6', '#d9e8f5', '#d9e7f5', '#d8e7f5', '#d7e6f5', '#d6e6f4', '#d6e5f4', '#d5e5f4', '#d4e4f4', '#d3e4f3', '#d3e3f3', '#d2e3f3', '#d1e2f3', '#d0e2f2', '#d0e1f2', '#cfe1f2', '#cee0f2', '#cde0f1', '#cddff1', '#ccdff1', '#cbdef1', '#cadef0', '#caddf0', '#c9ddf0', '#c8dcf0', '#c7dcef', '#c7dbef', '#c6dbef', '#c4daee', '#c3daee', '#c2d9ee', '#c1d9ed', '#bfd8ed', '#bed8ec', '#bdd7ec', '#bcd7eb', '#bad6eb', '#b9d6ea', '#b8d5ea', '#b7d4ea', '#b5d4e9', '#b4d3e9', '#b3d3e8', '#b2d2e8', '#b0d2e7', '#afd1e7', '#aed1e7', '#add0e6', '#abd0e6', '#aacfe5', '#a9cfe5', '#a8cee4', '#a6cee4', '#a5cde3', '#a4cce3', '#a3cce3', '#a1cbe2', '#a0cbe2', '#9fcae1', '#9dcae1', '#9cc9e1', '#9ac8e0', '#99c7e0', '#97c6df', '#95c5df', '#94c4df', '#92c4de', '#91c3de', '#8fc2de', '#8dc1dd', '#8cc0dd', '#8abfdd', '#89bedc', '#87bddc', '#85bcdc', '#84bcdb', '#82bbdb', '#81badb', '#7fb9da', '#7db8da', '#7cb7da', '#7ab6d9', '#79b5d9', '#77b5d9', '#75b4d8', '#74b3d8', '#72b2d8', '#71b1d7', '#6fb0d7', '#6dafd7', '#6caed6', '#6aaed6', '#69add5', '#68acd5', '#66abd4', '#65aad4', '#64a9d3', '#63a8d3', '#61a7d2', '#60a7d2', '#5fa6d1', '#5da5d1', '#5ca4d0', '#5ba3d0', '#5aa2cf', '#58a1cf', '#57a0ce', '#56a0ce', '#549fcd', '#539ecd', '#529dcc', '#519ccc', '#4f9bcb', '#4e9acb', '#4d99ca', '#4b98ca', '#4a98c9', '#4997c9', '#4896c8', '#4695c8', '#4594c7', '#4493c7', '#4292c6', '#4191c6', '#4090c5', '#3f8fc5', '#3e8ec4', '#3d8dc4', '#3c8cc3', '#3b8bc2', '#3a8ac2', '#3989c1', '#3888c1', '#3787c0', '#3686c0', '#3585bf', '#3484bf', '#3383be', '#3282be', '#3181bd', '#3080bd', '#2f7fbc', '#2e7ebc', '#2d7dbb', '#2c7cba', '#2b7bba', '#2a7ab9', '#2979b9', '#2777b8', '#2676b8', '#2575b7', '#2474b7', '#2373b6', '#2272b6', '#2171b5', '#2070b4', '#206fb4', '#1f6eb3', '#1e6db2', '#1d6cb1', '#1c6bb0', '#1c6ab0', '#1b69af', '#1a68ae', '#1967ad', '#1966ad', '#1865ac', '#1764ab', '#1663aa', '#1562a9', '#1561a9', '#1460a8', '#135fa7', '#125ea6', '#125da6', '#115ca5', '#105ba4', '#0f5aa3', '#0e59a2', '#0e58a2', '#0d57a1', '#0c56a0', '#0b559f', '#0a549e', '#0a539e', '#09529d', '#08519c', '#08509b', '#084f99', '#084e98', '#084d96', '#084c95', '#084b93', '#084a91', '#084990', '#08488e', '#08478d', '#08468b', '#08458a', '#084488', '#084387', '#084285', '#084184', '#084082', '#083e81', '#083d7f', '#083c7d', '#083b7c', '#083a7a', '#083979', '#083877', '#083776', '#083674', '#083573', '#083471', '#083370', '#08326e', '#08316d', '#08306b'],
    BlueToWhite: ['#f7fbff', '#f6faff', '#f5fafe', '#f5f9fe', '#f4f9fe', '#f3f8fe', '#f2f8fd', '#f2f7fd', '#f1f7fd', '#f0f6fd', '#eff6fc', '#eef5fc', '#eef5fc', '#edf4fc', '#ecf4fb', '#ebf3fb', '#eaf3fb', '#eaf2fb', '#e9f2fa', '#e8f1fa', '#e7f1fa', '#e7f0fa', '#e6f0f9', '#e5eff9', '#e4eff9', '#e3eef9', '#e3eef8', '#e2edf8', '#e1edf8', '#e0ecf8', '#dfecf7', '#dfebf7', '#deebf7', '#ddeaf7', '#dceaf6', '#dce9f6', '#dbe9f6', '#dae8f6', '#d9e8f5', '#d9e7f5', '#d8e7f5', '#d7e6f5', '#d6e6f4', '#d6e5f4', '#d5e5f4', '#d4e4f4', '#d3e4f3', '#d3e3f3', '#d2e3f3', '#d1e2f3', '#d0e2f2', '#d0e1f2', '#cfe1f2', '#cee0f2', '#cde0f1', '#cddff1', '#ccdff1', '#cbdef1', '#cadef0', '#caddf0', '#c9ddf0', '#c8dcf0', '#c7dcef', '#c7dbef', '#c6dbef', '#c4daee', '#c3daee', '#c2d9ee', '#c1d9ed', '#bfd8ed', '#bed8ec', '#bdd7ec', '#bcd7eb', '#bad6eb', '#b9d6ea', '#b8d5ea', '#b7d4ea', '#b5d4e9', '#b4d3e9', '#b3d3e8', '#b2d2e8', '#b0d2e7', '#afd1e7', '#aed1e7', '#add0e6', '#abd0e6', '#aacfe5', '#a9cfe5', '#a8cee4', '#a6cee4', '#a5cde3', '#a4cce3', '#a3cce3', '#a1cbe2', '#a0cbe2', '#9fcae1', '#9dcae1', '#9cc9e1', '#9ac8e0', '#99c7e0', '#97c6df', '#95c5df', '#94c4df', '#92c4de', '#91c3de', '#8fc2de', '#8dc1dd', '#8cc0dd', '#8abfdd', '#89bedc', '#87bddc', '#85bcdc', '#84bcdb', '#82bbdb', '#81badb', '#7fb9da', '#7db8da', '#7cb7da', '#7ab6d9', '#79b5d9', '#77b5d9', '#75b4d8', '#74b3d8', '#72b2d8', '#71b1d7', '#6fb0d7', '#6dafd7', '#6caed6', '#6aaed6', '#69add5', '#68acd5', '#66abd4', '#65aad4', '#64a9d3', '#63a8d3', '#61a7d2', '#60a7d2', '#5fa6d1', '#5da5d1', '#5ca4d0', '#5ba3d0', '#5aa2cf', '#58a1cf', '#57a0ce', '#56a0ce', '#549fcd', '#539ecd', '#529dcc', '#519ccc', '#4f9bcb', '#4e9acb', '#4d99ca', '#4b98ca', '#4a98c9', '#4997c9', '#4896c8', '#4695c8', '#4594c7', '#4493c7', '#4292c6', '#4191c6', '#4090c5', '#3f8fc5', '#3e8ec4', '#3d8dc4', '#3c8cc3', '#3b8bc2', '#3a8ac2', '#3989c1', '#3888c1', '#3787c0', '#3686c0', '#3585bf', '#3484bf', '#3383be', '#3282be', '#3181bd', '#3080bd', '#2f7fbc', '#2e7ebc', '#2d7dbb', '#2c7cba', '#2b7bba', '#2a7ab9', '#2979b9', '#2777b8', '#2676b8', '#2575b7', '#2474b7', '#2373b6', '#2272b6', '#2171b5', '#2070b4', '#206fb4', '#1f6eb3', '#1e6db2', '#1d6cb1', '#1c6bb0', '#1c6ab0', '#1b69af', '#1a68ae', '#1967ad', '#1966ad', '#1865ac', '#1764ab', '#1663aa', '#1562a9', '#1561a9', '#1460a8', '#135fa7', '#125ea6', '#125da6', '#115ca5', '#105ba4', '#0f5aa3', '#0e59a2', '#0e58a2', '#0d57a1', '#0c56a0', '#0b559f', '#0a549e', '#0a539e', '#09529d', '#08519c', '#08509b', '#084f99', '#084e98', '#084d96', '#084c95', '#084b93', '#084a91', '#084990', '#08488e', '#08478d', '#08468b', '#08458a', '#084488', '#084387', '#084285', '#084184', '#084082', '#083e81', '#083d7f', '#083c7d', '#083b7c', '#083a7a', '#083979', '#083877', '#083776', '#083674', '#083573', '#083471', '#083370', '#08326e', '#08316d', '#08306b'].reverse(),
    // Greens
    WhiteToGreen: ["#f7fcf5", "#f6fcf4", "#f6fcf4", "#f5fbf3", "#f5fbf2", "#f4fbf2", "#f4fbf1", "#f3faf0", "#f2faf0", "#f2faef", "#f1faee", "#f1faee", "#f0f9ed", "#f0f9ec", "#eff9ec", "#eef9eb", "#eef8ea", "#edf8ea", "#ecf8e9", "#ecf8e8", "#ebf7e7", "#ebf7e7", "#eaf7e6", "#e9f7e5", "#e9f6e4", "#e8f6e4", "#e7f6e3", "#e7f6e2", "#e6f5e1", "#e5f5e1", "#e4f5e0", "#e4f4df", "#e3f4de", "#e2f4dd", "#e1f4dc", "#e1f3dc", "#e0f3db", "#dff3da", "#def2d9", "#ddf2d8", "#ddf2d7", "#dcf1d6", "#dbf1d5", "#daf1d4", "#d9f0d3", "#d8f0d2", "#d7efd1", "#d6efd0", "#d5efcf", "#d4eece", "#d4eece", "#d3eecd", "#d2edcb", "#d1edca", "#d0ecc9", "#cfecc8", "#ceecc7", "#cdebc6", "#ccebc5", "#cbeac4", "#caeac3", "#c9eac2", "#c8e9c1", "#c6e9c0", "#c5e8bf", "#c4e8be", "#c3e7bd", "#c2e7bc", "#c1e6bb", "#c0e6b9", "#bfe6b8", "#bee5b7", "#bde5b6", "#bbe4b5", "#bae4b4", "#b9e3b3", "#b8e3b2", "#b7e2b0", "#b6e2af", "#b5e1ae", "#b3e1ad", "#b2e0ac", "#b1e0ab", "#b0dfaa", "#aedfa8", "#addea7", "#acdea6", "#abdda5", "#aadca4", "#a8dca3", "#a7dba2", "#a6dba0", "#a5da9f", "#a3da9e", "#a2d99d", "#a1d99c", "#9fd89b", "#9ed799", "#9dd798", "#9bd697", "#9ad696", "#99d595", "#97d494", "#96d492", "#95d391", "#93d390", "#92d28f", "#91d18e", "#8fd18d", "#8ed08c", "#8ccf8a", "#8bcf89", "#8ace88", "#88cd87", "#87cd86", "#85cc85", "#84cb84", "#82cb83", "#81ca82", "#80c981", "#7ec980", "#7dc87f", "#7bc77e", "#7ac77c", "#78c67b", "#77c57a", "#75c479", "#74c478", "#72c378", "#71c277", "#6fc276", "#6ec175", "#6cc074", "#6bbf73", "#69bf72", "#68be71", "#66bd70", "#65bc6f", "#63bc6e", "#62bb6e", "#60ba6d", "#5eb96c", "#5db86b", "#5bb86a", "#5ab769", "#58b668", "#57b568", "#56b467", "#54b466", "#53b365", "#51b264", "#50b164", "#4eb063", "#4daf62", "#4caf61", "#4aae61", "#49ad60", "#48ac5f", "#46ab5e", "#45aa5d", "#44a95d", "#42a85c", "#41a75b", "#40a75a", "#3fa65a", "#3ea559", "#3ca458", "#3ba357", "#3aa257", "#39a156", "#38a055", "#379f54", "#369e54", "#359d53", "#349c52", "#339b51", "#329a50", "#319950", "#30984f", "#2f974e", "#2e964d", "#2d954d", "#2b944c", "#2a934b", "#29924a", "#28914a", "#279049", "#268f48", "#258f47", "#248e47", "#238d46", "#228c45", "#218b44", "#208a43", "#1f8943", "#1e8842", "#1d8741", "#1c8640", "#1b8540", "#1a843f", "#19833e", "#18823d", "#17813d", "#16803c", "#157f3b", "#147e3a", "#137d3a", "#127c39", "#117b38", "#107a37", "#107937", "#0f7836", "#0e7735", "#0d7634", "#0c7534", "#0b7433", "#0b7332", "#0a7232", "#097131", "#087030", "#086f2f", "#076e2f", "#066c2e", "#066b2d", "#056a2d", "#05692c", "#04682b", "#04672b", "#04662a", "#03642a", "#036329", "#026228", "#026128", "#026027", "#025e27", "#015d26", "#015c25", "#015b25", "#015a24", "#015824", "#015723", "#005623", "#005522", "#005321", "#005221", "#005120", "#005020", "#004e1f", "#004d1f", "#004c1e", "#004a1e", "#00491d", "#00481d", "#00471c", "#00451c", "#00441b"],
    // Divergent
    Viridis: ["#440154", "#440256", "#450457", "#450559", "#46075a", "#46085c", "#460a5d", "#460b5e", "#470d60", "#470e61", "#471063", "#471164", "#471365", "#481467", "#481668", "#481769", "#48186a", "#481a6c", "#481b6d", "#481c6e", "#481d6f", "#481f70", "#482071", "#482173", "#482374", "#482475", "#482576", "#482677", "#482878", "#482979", "#472a7a", "#472c7a", "#472d7b", "#472e7c", "#472f7d", "#46307e", "#46327e", "#46337f", "#463480", "#453581", "#453781", "#453882", "#443983", "#443a83", "#443b84", "#433d84", "#433e85", "#423f85", "#424086", "#424186", "#414287", "#414487", "#404588", "#404688", "#3f4788", "#3f4889", "#3e4989", "#3e4a89", "#3e4c8a", "#3d4d8a", "#3d4e8a", "#3c4f8a", "#3c508b", "#3b518b", "#3b528b", "#3a538b", "#3a548c", "#39558c", "#39568c", "#38588c", "#38598c", "#375a8c", "#375b8d", "#365c8d", "#365d8d", "#355e8d", "#355f8d", "#34608d", "#34618d", "#33628d", "#33638d", "#32648e", "#32658e", "#31668e", "#31678e", "#31688e", "#30698e", "#306a8e", "#2f6b8e", "#2f6c8e", "#2e6d8e", "#2e6e8e", "#2e6f8e", "#2d708e", "#2d718e", "#2c718e", "#2c728e", "#2c738e", "#2b748e", "#2b758e", "#2a768e", "#2a778e", "#2a788e", "#29798e", "#297a8e", "#297b8e", "#287c8e", "#287d8e", "#277e8e", "#277f8e", "#27808e", "#26818e", "#26828e", "#26828e", "#25838e", "#25848e", "#25858e", "#24868e", "#24878e", "#23888e", "#23898e", "#238a8d", "#228b8d", "#228c8d", "#228d8d", "#218e8d", "#218f8d", "#21908d", "#21918c", "#20928c", "#20928c", "#20938c", "#1f948c", "#1f958b", "#1f968b", "#1f978b", "#1f988b", "#1f998a", "#1f9a8a", "#1e9b8a", "#1e9c89", "#1e9d89", "#1f9e89", "#1f9f88", "#1fa088", "#1fa188", "#1fa187", "#1fa287", "#20a386", "#20a486", "#21a585", "#21a685", "#22a785", "#22a884", "#23a983", "#24aa83", "#25ab82", "#25ac82", "#26ad81", "#27ad81", "#28ae80", "#29af7f", "#2ab07f", "#2cb17e", "#2db27d", "#2eb37c", "#2fb47c", "#31b57b", "#32b67a", "#34b679", "#35b779", "#37b878", "#38b977", "#3aba76", "#3bbb75", "#3dbc74", "#3fbc73", "#40bd72", "#42be71", "#44bf70", "#46c06f", "#48c16e", "#4ac16d", "#4cc26c", "#4ec36b", "#50c46a", "#52c569", "#54c568", "#56c667", "#58c765", "#5ac864", "#5cc863", "#5ec962", "#60ca60", "#63cb5f", "#65cb5e", "#67cc5c", "#69cd5b", "#6ccd5a", "#6ece58", "#70cf57", "#73d056", "#75d054", "#77d153", "#7ad151", "#7cd250", "#7fd34e", "#81d34d", "#84d44b", "#86d549", "#89d548", "#8bd646", "#8ed645", "#90d743", "#93d741", "#95d840", "#98d83e", "#9bd93c", "#9dd93b", "#a0da39", "#a2da37", "#a5db36", "#a8db34", "#aadc32", "#addc30", "#b0dd2f", "#b2dd2d", "#b5de2b", "#b8de29", "#bade28", "#bddf26", "#c0df25", "#c2df23", "#c5e021", "#c8e020", "#cae11f", "#cde11d", "#d0e11c", "#d2e21b", "#d5e21a", "#d8e219", "#dae319", "#dde318", "#dfe318", "#e2e418", "#e5e419", "#e7e419", "#eae51a", "#ece51b", "#efe51c", "#f1e51d", "#f4e61e", "#f6e620", "#f8e621", "#fbe723", "#fde725"],
    Inferno: ["#000004", "#010005", "#010106", "#010108", "#02010a", "#02020c", "#02020e", "#030210", "#040312", "#040314", "#050417", "#060419", "#07051b", "#08051d", "#09061f", "#0a0722", "#0b0724", "#0c0826", "#0d0829", "#0e092b", "#10092d", "#110a30", "#120a32", "#140b34", "#150b37", "#160b39", "#180c3c", "#190c3e", "#1b0c41", "#1c0c43", "#1e0c45", "#1f0c48", "#210c4a", "#230c4c", "#240c4f", "#260c51", "#280b53", "#290b55", "#2b0b57", "#2d0b59", "#2f0a5b", "#310a5c", "#320a5e", "#340a5f", "#360961", "#380962", "#390963", "#3b0964", "#3d0965", "#3e0966", "#400a67", "#420a68", "#440a68", "#450a69", "#470b6a", "#490b6a", "#4a0c6b", "#4c0c6b", "#4d0d6c", "#4f0d6c", "#510e6c", "#520e6d", "#540f6d", "#550f6d", "#57106e", "#59106e", "#5a116e", "#5c126e", "#5d126e", "#5f136e", "#61136e", "#62146e", "#64156e", "#65156e", "#67166e", "#69166e", "#6a176e", "#6c186e", "#6d186e", "#6f196e", "#71196e", "#721a6e", "#741a6e", "#751b6e", "#771c6d", "#781c6d", "#7a1d6d", "#7c1d6d", "#7d1e6d", "#7f1e6c", "#801f6c", "#82206c", "#84206b", "#85216b", "#87216b", "#88226a", "#8a226a", "#8c2369", "#8d2369", "#8f2469", "#902568", "#922568", "#932667", "#952667", "#972766", "#982766", "#9a2865", "#9b2964", "#9d2964", "#9f2a63", "#a02a63", "#a22b62", "#a32c61", "#a52c60", "#a62d60", "#a82e5f", "#a92e5e", "#ab2f5e", "#ad305d", "#ae305c", "#b0315b", "#b1325a", "#b3325a", "#b43359", "#b63458", "#b73557", "#b93556", "#ba3655", "#bc3754", "#bd3853", "#bf3952", "#c03a51", "#c13a50", "#c33b4f", "#c43c4e", "#c63d4d", "#c73e4c", "#c83f4b", "#ca404a", "#cb4149", "#cc4248", "#ce4347", "#cf4446", "#d04545", "#d24644", "#d34743", "#d44842", "#d54a41", "#d74b3f", "#d84c3e", "#d94d3d", "#da4e3c", "#db503b", "#dd513a", "#de5238", "#df5337", "#e05536", "#e15635", "#e25734", "#e35933", "#e45a31", "#e55c30", "#e65d2f", "#e75e2e", "#e8602d", "#e9612b", "#ea632a", "#eb6429", "#eb6628", "#ec6726", "#ed6925", "#ee6a24", "#ef6c23", "#ef6e21", "#f06f20", "#f1711f", "#f1731d", "#f2741c", "#f3761b", "#f37819", "#f47918", "#f57b17", "#f57d15", "#f67e14", "#f68013", "#f78212", "#f78410", "#f8850f", "#f8870e", "#f8890c", "#f98b0b", "#f98c0a", "#f98e09", "#fa9008", "#fa9207", "#fa9407", "#fb9606", "#fb9706", "#fb9906", "#fb9b06", "#fb9d07", "#fc9f07", "#fca108", "#fca309", "#fca50a", "#fca60c", "#fca80d", "#fcaa0f", "#fcac11", "#fcae12", "#fcb014", "#fcb216", "#fcb418", "#fbb61a", "#fbb81d", "#fbba1f", "#fbbc21", "#fbbe23", "#fac026", "#fac228", "#fac42a", "#fac62d", "#f9c72f", "#f9c932", "#f9cb35", "#f8cd37", "#f8cf3a", "#f7d13d", "#f7d340", "#f6d543", "#f6d746", "#f5d949", "#f5db4c", "#f4dd4f", "#f4df53", "#f4e156", "#f3e35a", "#f3e55d", "#f2e661", "#f2e865", "#f2ea69", "#f1ec6d", "#f1ed71", "#f1ef75", "#f1f179", "#f2f27d", "#f2f482", "#f3f586", "#f3f68a", "#f4f88e", "#f5f992", "#f6fa96", "#f8fb9a", "#f9fc9d", "#fafda1", "#fcffa4"],
    Magma: ["#000004", "#010005", "#010106", "#010108", "#020109", "#02020b", "#02020d", "#03030f", "#030312", "#040414", "#050416", "#060518", "#06051a", "#07061c", "#08071e", "#090720", "#0a0822", "#0b0924", "#0c0926", "#0d0a29", "#0e0b2b", "#100b2d", "#110c2f", "#120d31", "#130d34", "#140e36", "#150e38", "#160f3b", "#180f3d", "#19103f", "#1a1042", "#1c1044", "#1d1147", "#1e1149", "#20114b", "#21114e", "#221150", "#241253", "#251255", "#271258", "#29115a", "#2a115c", "#2c115f", "#2d1161", "#2f1163", "#311165", "#331067", "#341069", "#36106b", "#38106c", "#390f6e", "#3b0f70", "#3d0f71", "#3f0f72", "#400f74", "#420f75", "#440f76", "#451077", "#471078", "#491078", "#4a1079", "#4c117a", "#4e117b", "#4f127b", "#51127c", "#52137c", "#54137d", "#56147d", "#57157e", "#59157e", "#5a167e", "#5c167f", "#5d177f", "#5f187f", "#601880", "#621980", "#641a80", "#651a80", "#671b80", "#681c81", "#6a1c81", "#6b1d81", "#6d1d81", "#6e1e81", "#701f81", "#721f81", "#732081", "#752181", "#762181", "#782281", "#792282", "#7b2382", "#7c2382", "#7e2482", "#802582", "#812581", "#832681", "#842681", "#862781", "#882781", "#892881", "#8b2981", "#8c2981", "#8e2a81", "#902a81", "#912b81", "#932b80", "#942c80", "#962c80", "#982d80", "#992d80", "#9b2e7f", "#9c2e7f", "#9e2f7f", "#a02f7f", "#a1307e", "#a3307e", "#a5317e", "#a6317d", "#a8327d", "#aa337d", "#ab337c", "#ad347c", "#ae347b", "#b0357b", "#b2357b", "#b3367a", "#b5367a", "#b73779", "#b83779", "#ba3878", "#bc3978", "#bd3977", "#bf3a77", "#c03a76", "#c23b75", "#c43c75", "#c53c74", "#c73d73", "#c83e73", "#ca3e72", "#cc3f71", "#cd4071", "#cf4070", "#d0416f", "#d2426f", "#d3436e", "#d5446d", "#d6456c", "#d8456c", "#d9466b", "#db476a", "#dc4869", "#de4968", "#df4a68", "#e04c67", "#e24d66", "#e34e65", "#e44f64", "#e55064", "#e75263", "#e85362", "#e95462", "#ea5661", "#eb5760", "#ec5860", "#ed5a5f", "#ee5b5e", "#ef5d5e", "#f05f5e", "#f1605d", "#f2625d", "#f2645c", "#f3655c", "#f4675c", "#f4695c", "#f56b5c", "#f66c5c", "#f66e5c", "#f7705c", "#f7725c", "#f8745c", "#f8765c", "#f9785d", "#f9795d", "#f97b5d", "#fa7d5e", "#fa7f5e", "#fa815f", "#fb835f", "#fb8560", "#fb8761", "#fc8961", "#fc8a62", "#fc8c63", "#fc8e64", "#fc9065", "#fd9266", "#fd9467", "#fd9668", "#fd9869", "#fd9a6a", "#fd9b6b", "#fe9d6c", "#fe9f6d", "#fea16e", "#fea36f", "#fea571", "#fea772", "#fea973", "#feaa74", "#feac76", "#feae77", "#feb078", "#feb27a", "#feb47b", "#feb67c", "#feb77e", "#feb97f", "#febb81", "#febd82", "#febf84", "#fec185", "#fec287", "#fec488", "#fec68a", "#fec88c", "#feca8d", "#fecc8f", "#fecd90", "#fecf92", "#fed194", "#fed395", "#fed597", "#fed799", "#fed89a", "#fdda9c", "#fddc9e", "#fddea0", "#fde0a1", "#fde2a3", "#fde3a5", "#fde5a7", "#fde7a9", "#fde9aa", "#fdebac", "#fcecae", "#fceeb0", "#fcf0b2", "#fcf2b4", "#fcf4b6", "#fcf6b8", "#fcf7b9", "#fcf9bb", "#fcfbbd", "#fcfdbf"]
};

/** Text format functions */

export const getEqualIntervals = (value: number): string => {
    const quintile = Math.floor(value / 20);
    // Prevent 100-120
    if (quintile === 5) {
        return '80-100';
    }
    return `${quintile * 20}-${(quintile + 1) * 20}`;
}

/** Scale functions */

export const continuousScale: ScaleMethod = (values) => {
    if (!values?.length) return [0, 1];
    return [values[0], values[values.length - 1]];
};

export const equalCountScale: ScaleMethod = (values, steps = 5) => {
    if (!values?.length) return [0, 1];
    const n = steps;
    const quants = [];
    for (let i = 0; i <= n; i++) {
        const idx = Math.min(Math.floor((i / n) * (values.length - 1)), values.length - 1);
        quants.push(values[idx]);
    }
    return quants;
};

export const equalIntervalScale: ScaleMethod = (values, steps = 5) => {
    if (!values?.length) return [0, 1];
    const min = values[0];
    const max = values[values.length - 1];
    const intervals = [];
    for (let i = 0; i <= steps; i++) {
        intervals.push(min + (max - min) * (i / steps));
    }
    return intervals;
};

export const logarithmicScale: ScaleMethod = (values, steps = 5) => {
    if (!values?.length) return [0, 1];
    const min = values[0];
    const max = values[values.length - 1];
    console.log("logarithmicScale", { values, min, max });
    // Log scales require values > 0. Shift values to ensure they are > 0.
    const offset = min <= 0 ? Math.abs(min) + 1 : 0;
    const logMin = Math.log(min + offset);
    const logMax = Math.log(max + offset);
    const logScale = [];
    for (let i = 0; i <= steps; i++) {
        const val = Math.exp(logMin + (logMax - logMin) * (i / steps)) - offset;
        logScale.push(val);
    }
    // Ensure first and last are exact min and max to counteract floating point drifts
    logScale[0] = min;
    logScale[steps] = max;

    return logScale;
};

const METRIC_DATA: Record<string, Omit<MetricDef, 'category'>[]> = {
    'metrics.categories.mobility_poverty_index': [
        {
            id: 'IMPT_entropy_pca',
            label: 'metrics.impt_entropy.label',
            description: 'metrics.impt_entropy.description',
            pallete: COLORS.GreenToRed,
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            //sources: ['census', 'imob', 'osm', 'ansr', 'pmus', 'gba', 'gtfs']
        },
        {
            id: 'IMPT_score_pca_geom',
            label: 'metrics.impt_geom.label',
            description: 'metrics.impt_geom.description',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.GreenToRed,
            default: true
            //sources: ['census', 'imob', 'osm', 'ansr', 'pmus', 'gba', 'gtfs']
        },
        {
            id: 'IMPT_dynamic',
            label: 'metrics.impt_dynamic.label',
            description: 'metrics.impt_dynamic.description',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.GreenToRed,
            isCalculated: true
        },
    ],
    'metrics.categories.dimensions': [
        {
            id: 'Mobility_Index',
            label: 'metrics.mobility.label', icon: '🚲',
            description: 'metrics.mobility.description',
            format: (v, _min, _max) => getEqualIntervals(v || 0),
            scaleMethod: continuousScale,
            pallete: COLORS.RedToGreen,
            isContributory: true, defaultWeight: 0.25,
            auxiliaryDataUrl: 'https://ushift.tecnico.ulisboa.pt/content/impt/champions.json',
            renderAuxiliaryData: (data: any, metricId: string, t: (key: string) => string, limit = 3) => {
                const list = data[metricId] || [];
                if (!list.length) return null;
                return (
                    <div className="mt-2 p-3 bg-neutral-500/10 rounded-xl">
                        <h4 className="text-[10px] font-black uppercase mb-2 opacity-50 tracking-widest">{t('metrics.categories.top_contributors')}</h4>
                        <ul className="text-[11px] space-y-1">
                            {list.slice(0, limit).map((item: string) => (
                                <li key={item} className="opacity-80 flex items-start gap-2">
                                    <span className="text-sky-800">•</span>
                                    <span className="truncate">{t(`metrics.${item}.label`) !== `metrics.${item}.label` ? t(`metrics.${item}.label`) : (t(`variations.${item}`) !== `variations.${item}` ? t(`variations.${item}`) : item.replace(/_/g, ' '))}</span>
                                </li>
                            ))}
                        </ul>
                    </div>
                );
            }
        },
        {
            id: 'Accessibility_Index',
            label: 'metrics.accessibility.label', icon: '🏘️',
            description: 'metrics.accessibility.description',
            format: (v, _min, _max) => getEqualIntervals(v || 0),
            scaleMethod: continuousScale,
            pallete: COLORS.RedToGreen,
            isContributory: true, defaultWeight: 0.25,
            auxiliaryDataUrl: 'https://ushift.tecnico.ulisboa.pt/content/impt/champions.json',
            renderAuxiliaryData: (data: any, metricId: string, t: (key: string) => string, limit = 3) => {
                const list = data[metricId] || [];
                if (!list.length) return null;
                return (
                    <div className="mt-2 p-3 bg-neutral-500/10 rounded-xl">
                        <h4 className="text-[10px] font-black uppercase mb-2 opacity-50 tracking-widest">{t('metrics.categories.top_contributors')}</h4>
                        <ul className="text-[11px] space-y-1">
                            {list.slice(0, limit).map((item: string) => (
                                <li key={item} className="opacity-80 flex items-start gap-2">
                                    <span className="text-sky-800">•</span>
                                    <span className="truncate">{t(`metrics.${item}.label`) !== `metrics.${item}.label` ? t(`metrics.${item}.label`) : (t(`variations.${item}`) !== `variations.${item}` ? t(`variations.${item}`) : item.replace(/_/g, ' '))}</span>
                                </li>
                            ))}
                        </ul>
                    </div>
                );
            }
        },
        {
            id: 'Safety_Index',
            label: 'metrics.safety.label', icon: '🛡️',
            description: 'metrics.safety.description',
            format: (v, _min, _max) => getEqualIntervals(v || 0),
            scaleMethod: continuousScale,
            pallete: COLORS.RedToGreen,
            isContributory: true, defaultWeight: 0.25,
            auxiliaryDataUrl: 'https://ushift.tecnico.ulisboa.pt/content/impt/champions.json',
            renderAuxiliaryData: (data: any, metricId: string, t: (key: string) => string, limit = 3) => {
                const list = data[metricId] || [];
                if (!list.length) return null;
                return (
                    <div className="mt-2 p-3 bg-neutral-500/10 rounded-xl">
                        <h4 className="text-[10px] font-black uppercase mb-2 opacity-50 tracking-widest">{t('metrics.categories.top_contributors')}</h4>
                        <ul className="text-[11px] space-y-1">
                            {list.slice(0, limit).map((item: string) => (
                                <li key={item} className="opacity-80 flex items-start gap-2">
                                    <span className="text-sky-800">•</span>
                                    <span className="truncate">{t(`metrics.${item}.label`) !== `metrics.${item}.label` ? t(`metrics.${item}.label`) : (t(`variations.${item}`) !== `variations.${item}` ? t(`variations.${item}`) : item.replace(/_/g, ' '))}</span>
                                </li>
                            ))}
                        </ul>
                    </div>
                );
            }
        },
        {
            id: 'Affordability_Index',
            label: 'metrics.affordability.label', icon: '💰',
            description: 'metrics.affordability.description',
            format: (v, _min, _max) => getEqualIntervals(v || 0),
            scaleMethod: continuousScale,
            pallete: COLORS.RedToGreen,
            isContributory: true, defaultWeight: 0.25,
            sources: ['ansr', 'pmus', 'osm', 'ine', 'iptoll', 'accounting']
        }
    ],
    'metrics.categories.mobility': [
        {
            id: 'modal_census_share',
            label: 'metrics.modal_census_share.label',
            description: 'metrics.modal_census_share.description',
            icon: '📊💼',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToGreen,
            sources: ['census']
        },
        {
            id: 'modal_imob_share',
            label: 'metrics.modal_imob_share.label',
            description: 'metrics.modal_imob_share.description',
            icon: '📊',
            format: (v, _min, _max) => (v || 0).toFixed(1), unit: '%',
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToGreen,
            sources: ['imob']
        },
        {
            id: 'mobility_commuting_avg_tt',
            label: 'metrics.mobility_commuting_avg_tt.label',
            description: 'metrics.mobility_commuting_avg_tt.description',
            icon: '⌛💼',
            format: (v, _min, _max) => Math.round(v || 0).toString(), unit: 'min',
            scaleMethod: continuousScale,
            pallete: COLORS.GreenToRed,
            sources: ['imob', 'gba']
        },
        {
            id: 'mobility_cost{poi_type}{n_transfers}{n_opportunities}{population}',
            id_variations: {
                poi_type: ['_health', '_health_primary', '_health_hospital', '_groceries', '_greenspaces', '_recreation', '_schools_primary'],
                population: {
                    options: ['_residents', '_elder', '_kids', '_active', '_young'],
                    viewLevels: ['freguesia', 'municipality']
                },
                n_opportunities: {
                    options: ['_n1', '_n2', '_n3'],
                    formSlider: true
                },
                n_transfers: {
                    options: ['_1t', '_2t'],
                    modes: ['pt']
                }

            },
            label: 'metrics.mobility_cost.label',
            description: 'metrics.mobility_cost.description',
            icon: '⌛',
            format: (v, _min, _max) => Math.round(v || 0).toString(), unit: 'min',
            scaleMethod: continuousScale,
            pallete: COLORS.GreenToRed,
            sources: ['pmus', 'gba', 'osm']
        },
        {
            id: 'mobility_shared_mobility_points',
            label: 'metrics.mobility_shared_mobility_points.label',
            description: 'metrics.mobility_shared_mobility_points.description',
            icon: '🛴',
            format: (v, _min, _max) => (v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.Inferno.reverse(),
            sources: ['pmus']
        },
        {
            id: 'mobility_stop_coverage_ratio_served_population',
            label: 'metrics.mobility_stop_coverage_ratio_served_population.label',
            description: 'metrics.mobility_stop_coverage_ratio_served_population.description',
            icon: '🚏',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToGreen,
            sources: ['gtfs', 'census']
        }
    ],
    'metrics.categories.accessibility': [
        {
            id: 'access{poi_type}{n_transfers}{n_time}{population}',
            id_variations: {
                poi_type: ['_health', '_health_primary', '_health_hospital', '_groceries', '_greenspaces', '_recreation', '_schools_primary'],
                population: {
                    options: ['_residents', '_elder', '_kids', '_active', '_young'],
                    viewLevels: ['freguesia', 'municipality']
                },
                n_time: {
                    options: ['_5min', '_10min', '_15min', '_30min', '_45min', '_60min', '_75min', '_90min'],
                    formSlider: true
                },
                n_transfers: {
                    options: ['_1t', '_2t'],
                    modes: ['pt']
                }

            },
            label: 'metrics.access.label',
            description: 'metrics.access.description',
            icon: '📍⌛',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.Viridis.reverse(),
            sources: ['pmus', 'gba', 'osm']
        },
        {
            id: 'pois_n{poi_type}',
            id_variations: {
                poi_type: ['_jobs', '_health', '_health_primary', '_health_hospital', '_groceries', '_greenspaces', '_recreation', '_schools_primary', '_schools_high', '_pt']
            },
            label: 'metrics.pois.label',
            description: 'metrics.pois.description',
            icon: '📍',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.Magma.reverse(),
            sources: ['pmus', 'gba', 'osm']
        },
        {
            id: 'access_gap_time_accessibility_gap',
            label: 'metrics.access_gap_time_accessibility_gap.label',
            description: 'metrics.access_gap_time_accessibility_gap.description',
            icon: '⚖️⌛',
            format: (v, _min, _max) => (v || 0).toFixed(2), ignoreValues: [0, null], unit: 'min',
            scaleMethod: continuousScale, scaleMinEqualsMax: true,
            pallete: COLORS.GreenToRed,
            sources: ['pmus', 'gba']
        },
        {
            id: 'access_gap_time_relative_gap_time',
            label: 'metrics.access_gap_time_relative_gap_time.label',
            description: 'metrics.access_gap_time_relative_gap_time.description',
            icon: '⚖️⏳',
            format: (v, _min, _max) => (v || 0).toFixed(2), ignoreValues: [null],
            scaleMethod: continuousScale, scaleMinEqualsMax: false,
            pallete: COLORS.GreenToRed,
            sources: ['pmus', 'gba']
        }
    ],
    'metrics.categories.safety': [
        {
            id: 'safety_inner_total_acidentes',
            label: 'metrics.safety_inner_total_acidentes.label',
            description: 'metrics.safety_inner_total_acidentes.description',
            icon: '💥',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: logarithmicScale, steps: 10, ignoreValues: [0, null],
            pallete: COLORS.WhiteToRed,
            sources: ['ansr', 'pmus']
        },
        {
            id: 'safety_inner_acidentes_per_1000res',
            label: 'metrics.safety_inner_acidentes_per_1000res.label',
            description: 'metrics.safety_inner_acidentes_per_1000res.description',
            icon: '⚠️',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale, ignoreValues: [0, null],
            pallete: COLORS.WhiteToRed,
            showAlwaysOnDetails: true,
            sources: ['ansr', 'pmus', 'census']
        },
        {
            id: 'safety_inner_vitimas_mortais30',
            label: 'metrics.safety_inner_vitimas_mortais30.label',
            description: 'metrics.safety_inner_vitimas_mortais30.description',
            icon: '🚑',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale, ignoreValues: [0, null],
            pallete: COLORS.WhiteToRed,
            sources: ['ansr', 'pmus']
        },
        {
            id: 'safety_inner_indice_gravidade',
            label: 'metrics.safety_inner_indice_gravidade.label',
            description: 'metrics.safety_inner_indice_gravidade.description',
            icon: '🤕',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.WhiteToRed,
            sources: ['ansr', 'pmus']
        },
        {
            id: 'safety_inner_total_veiculos',
            label: 'metrics.safety_inner_total_veiculos.label',
            description: 'metrics.safety_inner_total_veiculos.description',
            icon: '🚗',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: logarithmicScale, steps: 10, ignoreValues: [0, null],
            pallete: COLORS.WhiteToRed,
            sources: ['ansr', 'pmus']
        }
    ],
    'metrics.categories.affordability': [
        {
            id: 'affordability_total_money',
            label: 'metrics.affordability_total_money.label',
            description: 'metrics.affordability_total_money.description',
            icon: '💶💼',
            format: (v, _min, _max) => (v || 0).toFixed(2), ignoreValues: [0, null],
            scaleMethod: continuousScale, unit: '€',
            scaleMin: 0,
            pallete: COLORS.Viridis.reverse(),
            sources: ['iptoll', 'accounting']
        },
        {
            id: 'access_gap_money_cost_gap',
            label: 'metrics.access_gap_money_cost_gap.label',
            description: 'metrics.access_gap_money_cost_gap.description',
            icon: '⚖️🎟️',
            format: (v, _min, _max) => (v || 0).toFixed(2), ignoreValues: [0, null], unit: '€',
            scaleMethod: continuousScale, scaleMinEqualsMax: true,
            pallete: COLORS.GreenToRed,
            sources: ['iptoll', 'accounting']
        },
        {
            id: 'access_gap_money_relative_gap_cost',
            label: 'metrics.access_gap_money_relative_gap_cost.label',
            description: 'metrics.access_gap_money_relative_gap_cost.description',
            icon: '⚖️🎟️',
            format: (v, _min, _max) => (v || 0).toFixed(2), ignoreValues: [null],
            scaleMethod: continuousScale, scaleMinEqualsMax: true,
            pallete: COLORS.GreenToRed,
            sources: ['iptoll', 'accounting']
        },
        {
            id: 'census_income_income_hh',
            label: 'metrics.census_income_income_hh.label',
            description: 'metrics.census_income_income_hh.description',
            icon: '💰',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale, ignoreValues: [null], unit: "€",
            pallete: COLORS.Viridis.reverse(),
            showAlwaysOnDetails: true,
            sources: ['ine']
        },
        {
            id: 'census_income_gini_coef',
            label: 'metrics.census_income_gini_coef.label',
            description: 'metrics.census_income_gini_coef.description',
            icon: '⚖️',
            format: (v, _min, _max) => Math.round(v || 0).toString(), unit: "%",
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.Viridis.reverse(),
            sources: ['ine']
        },
        {
            id: 'census_income_housing_costs',
            label: 'metrics.census_income_housing_costs.label',
            description: 'metrics.census_income_housing_costs.description',
            icon: '🏠💸',
            format: (v, _min, _max) => Math.round(v || 0).toString(), unit: "€",
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.Viridis.reverse(),
            sources: ['census']
        },
        // affordability_transp_inc_comp_pass, affordability_transp_inc_comp_sf, affordability_h_transp_inc_comp_pass, affordability_h_transp_inc_comp_sf
        {
            id: 'affordability_transp_inc_comp',
            label: 'metrics.affordability_transp_inc_comp.label',
            description: 'metrics.affordability_transp_inc_comp.description',
            icon: '💸🚗🚌',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.Viridis.reverse(),
            sources: ['ine', 'census', 'iptoll', 'accounting']
        },
        {
            id: 'affordability_h_transp_inc_comp',
            label: 'metrics.affordability_h_transp_inc_comp.label',
            description: 'metrics.affordability_h_transp_inc_comp.description',
            icon: '💸🚗🚌🏠',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.Viridis.reverse(),
            sources: ['ine', 'census', 'iptoll', 'accounting']
        },
        {
            id: 'veh_ownership_total_motor_vehicles_per_hh',
            label: 'metrics.veh_ownership_total_motor_vehicles_per_hh.label',
            description: 'metrics.veh_ownership_total_motor_vehicles_per_hh.description',
            icon: '🚗🛵',
            format: (v, _min, _max) => Math.round(v || 0).toString(), unit: 'vh',
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.Viridis.reverse(),
            sources: ['imob']
        },
        {
            id: 'veh_ownership_pct_hh_no_vehicle',
            label: 'metrics.veh_ownership_pct_hh_no_vehicle.label',
            description: 'metrics.veh_ownership_pct_hh_no_vehicle.description',
            icon: '🚶',
            format: (v, _min, _max) => Math.round(v || 0).toString(), unit: '%',
            scaleMethod: continuousScale, ignoreValues: [null],
            pallete: COLORS.Viridis.reverse(),
            showAlwaysOnDetails: true,
            sources: ['imob']
        }
    ],
    'metrics.categories.census_landuse': [
        {
            id: 'census_landuse_population',
            label: 'metrics.census_landuse_population.label',
            description: 'metrics.census_landuse_population.description',
            icon: '👥',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            showAlwaysOnDetails: true,
            sources: ['census']
        },
        {
            id: 'census_landuse_population_density',
            label: 'metrics.census_landuse_population_density.label',
            description: 'metrics.census_landuse_population_density.description',
            icon: '🏙️',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['census']
        },
        {
            id: 'census_landuse_youth_ratio',
            label: 'metrics.census_landuse_youth_ratio.label',
            description: 'metrics.census_landuse_youth_ratio.description',
            icon: '👶',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['census']
        },
        {
            id: 'census_landuse_elderly_ratio',
            label: 'metrics.census_landuse_elderly_ratio.label',
            description: 'metrics.census_landuse_elderly_ratio.description',
            icon: '👵',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['census']
        },
        {
            id: 'census_landuse_women_percentage',
            label: 'metrics.census_landuse_women_percentage.label',
            description: 'metrics.census_landuse_women_percentage.description',
            icon: '👩',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['census']
        },
        {
            id: 'mobility_infrastructure_road_length',
            label: 'metrics.mobility_infrastructure_road_length.label',
            description: 'metrics.mobility_infrastructure_road_length.description',
            icon: '🛣️',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['osm']
        },
        {
            id: 'mobility_infrastructure_cycleway_length',
            label: 'metrics.mobility_infrastructure_cycleway_length.label',
            description: 'metrics.mobility_infrastructure_cycleway_length.description',
            icon: '🚲',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['osm']
        },
        {
            id: 'mobility_infrastructure_cycleway_to_road_ratio',
            label: 'metrics.mobility_infrastructure_cycleway_to_road_ratio.label',
            description: 'metrics.mobility_infrastructure_cycleway_to_road_ratio.description',
            icon: '🛤️',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['osm']
        },
        {
            id: 'mobility_infrastructure_pedpath_length',
            label: 'metrics.mobility_infrastructure_pedpath_length.label',
            description: 'metrics.mobility_infrastructure_pedpath_length.description',
            icon: '🚶',
            format: (v, _min, _max) => Math.round(v || 0).toString(),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['osm']
        },
        {
            id: 'mobility_infrastructure_pedpath_to_road_ratio',
            label: 'metrics.mobility_infrastructure_pedpath_to_road_ratio.label',
            description: 'metrics.mobility_infrastructure_pedpath_to_road_ratio.description',
            icon: '👟',
            format: (v, _min, _max) => Math.round((v || 0) * 100).toString(), unit: '%',
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['osm']
        },
        {
            id: 'census_landuse_buildings_pre1945_percentage',
            label: 'metrics.census_landuse_buildings_pre1945_percentage.label',
            description: 'metrics.census_landuse_buildings_pre1945_percentage.description',
            icon: '🏚️',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['census']
        },
        {
            id: 'census_landuse_volume_density',
            label: 'metrics.census_landuse_volume_density.label',
            description: 'metrics.census_landuse_volume_density.description',
            icon: '🏢',
            format: (v, _min, _max) => (v || 0).toFixed(1),
            scaleMethod: continuousScale,
            pallete: COLORS.WhiteToBlue,
            sources: ['gba']
        }
    ]
};

export const METRICS = Object.freeze(
    Object.fromEntries(
        Object.entries(METRIC_DATA).map(([category, metrics]) => [
            category,
            metrics.map(m => ({ ...m, category } as MetricDef))
        ])
    )
) as Record<string, MetricDef[]>;


export const FLAT_METRICS = Object.values(METRICS).flat();

export const REGIONS = {
    'metropolis': { name: "regions.metropolis", center: [38.74, -9.10], zoom: 11 },
    'PT1B': { name: "regions.grand_lisbon", center: [38.85, -9.15], zoom: 11 },
    'PT1A': { name: "regions.setubal_peninsula", center: [38.65, -8.90], zoom: 11 }
} as const;

export const MODES = [
    // suffix is added to METRICS.id to get the metric for that mode
    // > Based on it we can determine if the metric exists for that mode and display the mode selector accordingly
    // suffixFallback is used only on IMPT dynamic computation 
    // > It tells that for that mode, if no metric with suffix, suffixFallback metric should be used instead 
    { id: 'all', label: 'modes.all', suffix: '', icon: '🌐' },
    { id: 'all_pass', label: 'modes.all_pass', suffix: '_pass', suffixFallback: '', icon: '🌐' },
    { id: 'all_no_pass', label: 'modes.all_no_pass', suffix: '_no_pass', suffixFallback: '', icon: '🌐' },
    { id: 'pt', label: 'modes.pt', suffix: '_pt', icon: '🚍' },
    { id: 'pt_pass', label: 'modes.pt_pass', suffix: '_pt_pass', suffixFallback: '_pt', icon: '🚍' },
    { id: 'pt_no_pass', label: 'modes.pt_no_pass', suffix: '_pt_no_pass', suffixFallback: '_pt', icon: '🚍' },
    { id: 'walk', label: 'modes.walk', suffix: '_walk', icon: '🚶' },
    { id: 'bike', label: 'modes.bike', suffix: '_bike', icon: '🚲' },
    { id: 'car', label: 'modes.car', suffix: '_car', icon: '🚗' }
] as const;

export type ModeId = (typeof MODES)[number]['id'];
export type RegionKey = keyof typeof REGIONS;
export const REGION_KEYS = Object.keys(REGIONS) as RegionKey[];
export const DEFAULT_REGION: RegionKey = REGION_KEYS[0];

export const MAP_LAYERS = [
    {
        id: 'carto',
        label: 'map.layer_carto',
        icon: '',
        attribution: '&copy; CARTO',
        getUrl: (isDark: boolean) => isDark
            ? "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png"
            : "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
    },
    {
        id: 'satellite',
        label: 'map.layer_satellite',
        icon: '',
        attribution: '&copy; ESRI',
        url: "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    },
    {
        id: 'osm',
        label: 'map.layer_osm',
        icon: '',
        attribution: '&copy; OpenStreetMap',
        url: "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    }
];

