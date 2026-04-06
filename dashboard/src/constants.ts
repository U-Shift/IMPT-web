import { MetricDef, ViewLevel } from './types';

export const LEVEL_CONFIG: Record<ViewLevel, { file: string, parent?: ViewLevel }> = {
    'municipality': { file: 'data/municipios_aggregated.geojson' },
    'freguesia': { file: 'data/freguesias_aggregated.geojson', parent: 'municipality' },
    'hex': { file: 'data/grid_aggregated.geojson', parent: 'freguesia' }
};

export const getEqualIntervals = (value: number): string => {
    const quintile = Math.floor(value / 20);
    // Prevent 100-120
    if (quintile === 5) {
        return '80-100';
    }
    return `${quintile * 20}-${(quintile + 1) * 20}`;
}

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

    Divergent: {
        negative: ['#006837', '#31a354', '#78c679', '#c2e699', '#f7fcb9'],
        positive: ['#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#f03b20', '#bd0026']
    }
};

const METRIC_DATA: Record<string, Omit<MetricDef, 'category'>[]> = {
    'metrics.categories.mobility_poverty_index': [
        {
            id: 'IMPT_entropy_pca',
            label: 'metrics.impt_entropy.label',
            description: 'metrics.impt_entropy.description',
            pallete: COLORS.GreenToRed,
            format: (v, min, max) => (v || 0).toFixed(1),
            showDetails: true, showDetailsOnlyWhenSelected: true
        },
        {
            id: 'IMPT_score_pca_geom',
            label: 'metrics.impt_geom.label',
            description: 'metrics.impt_geom.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed,
            showDetails: true, showDetailsOnlyWhenSelected: true, default: true
        },
        {
            id: 'IMPT_dynamic',
            label: 'metrics.impt_dynamic.label',
            description: 'metrics.impt_dynamic.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed,
            showDetails: true, showDetailsOnlyWhenSelected: true,
            isCalculated: true
        },
    ],
    'metrics.categories.dimensions': [
        {
            id: 'Accessibility_Index',
            label: 'metrics.accessibility.label', icon: '🏘️',
            description: 'metrics.accessibility.description',
            format: (v, min, max) => getEqualIntervals(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Mobility_Index',
            label: 'metrics.mobility.label', icon: '🚲',
            description: 'metrics.mobility.description',
            format: (v, min, max) => getEqualIntervals(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Safety_Index',
            label: 'metrics.safety.label', icon: '🛡️',
            description: 'metrics.safety.description',
            format: (v, min, max) => getEqualIntervals(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        },
        {
            id: 'Affordability_Index',
            label: 'metrics.affordability.label', icon: '💰',
            description: 'metrics.affordability.description',
            format: (v, min, max) => getEqualIntervals(v || 0),
            pallete: COLORS.RedToGreen,
            showDetails: true,
            isContributory: true, defaultWeight: 0.25
        }
    ],
    'metrics.categories.safety': [
        // safety_total_acidentes, safety_indice_gravidade, safety_inner_total_acidentes, safety_inner_indice_gravidade
        {
            id: 'safety_total_acidentes',
            label: 'metrics.safety_total_acidentes',
            description: 'metrics.safety_total_acidentes.description',
            format: (v, min, max) => Math.round(v || 0).toString(), quantiles: 100, ignoreValues: [0, null],
            pallete: COLORS.WhiteToRed
        },
        {
            id: 'safety_indice_gravidade',
            label: 'metrics.safety_indice_gravidade',
            description: 'metrics.safety_indice_gravidade.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToRed
        },
        {
            id: 'safety_inner_total_acidentes',
            label: 'metrics.safety_inner_total_acidentes',
            description: 'metrics.safety_inner_total_acidentes.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.WhiteToRed
        },
        {
            id: 'safety_inner_indice_gravidade',
            label: 'metrics.safety_inner_indice_gravidade',
            description: 'metrics.safety_inner_indice_gravidade.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToRed
        }
    ],
    'metrics.categories.affordability': [
        {
            id: 'affordability_total_money',
            label: 'metrics.affordability_total_money.label',
            description: 'metrics.affordability_total_money.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.GreenToRed
        }
    ],
    // census
    'metrics.categories.census_population': [
        {
            id: 'modal_census_share',
            label: 'metrics.modal_census_share',
            description: 'metrics.modal_census_share.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_income_income_hh',
            label: 'metrics.census_income_income_hh',
            description: 'metrics.census_income_income_hh.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_income_gini_coef',
            label: 'metrics.census_income_gini_coef',
            description: 'metrics.census_income_gini_coef.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        },
        {
            id: 'census_income_housing_costs',
            label: 'metrics.census_income_housing_costs',
            description: 'metrics.census_income_housing_costs.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.RedToGreen
        }
    ],
    'metrics.categories.census_landuse': [
        {
            id: 'census_landuse_population',
            label: 'metrics.census_landuse_population.label',
            description: 'metrics.census_landuse_population.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_population_density',
            label: 'metrics.census_landuse_population_density.label',
            description: 'metrics.census_landuse_population_density.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_youth_ratio',
            label: 'metrics.census_landuse_youth_ratio.label',
            description: 'metrics.census_landuse_youth_ratio.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_elderly_ratio',
            label: 'metrics.census_landuse_elderly_ratio.label',
            description: 'metrics.census_landuse_elderly_ratio.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_women_percentage',
            label: 'metrics.census_landuse_women_percentage.label',
            description: 'metrics.census_landuse_women_percentage.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'mobility_infrastructure_road_length',
            label: 'metrics.mobility_infrastructure_road_length.label',
            description: 'metrics.mobility_infrastructure_road_length.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'mobility_infrastructure_cycleway_length',
            label: 'metrics.mobility_infrastructure_cycleway_length.label',
            description: 'metrics.mobility_infrastructure_cycleway_length.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'mobility_infrastructure_cycleway_to_road_ratio',
            label: 'metrics.mobility_infrastructure_cycleway_to_road_ratio.label',
            description: 'metrics.mobility_infrastructure_cycleway_to_road_ratio.description',
            format: (v, min, max) => (v || 0).toFixed(2),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'mobility_infrastructure_pedpath_length',
            label: 'metrics.mobility_infrastructure_pedpath_length.label',
            description: 'metrics.mobility_infrastructure_pedpath_length.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'mobility_infrastructure_pedpath_to_road_ratio',
            label: 'metrics.mobility_infrastructure_pedpath_to_road_ratio.label',
            description: 'metrics.mobility_infrastructure_pedpath_to_road_ratio.description',
            format: (v, min, max) => (v || 0).toFixed(2),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_buildings',
            label: 'metrics.census_landuse_buildings.label',
            description: 'metrics.census_landuse_buildings.description',
            format: (v, min, max) => Math.round(v || 0).toString(),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_buildings_pre1945_percentage',
            label: 'metrics.census_landuse_buildings_pre1945_percentage.label',
            description: 'metrics.census_landuse_buildings_pre1945_percentage.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToBlue
        },
        {
            id: 'census_landuse_volume_density',
            label: 'metrics.census_landuse_volume_density.label',
            description: 'metrics.census_landuse_volume_density.description',
            format: (v, min, max) => (v || 0).toFixed(1),
            pallete: COLORS.WhiteToBlue
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
