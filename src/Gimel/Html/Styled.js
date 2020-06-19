const styled = require ("styled-components").default

exports.styledRaw = tag => css => styled[tag]`${css}`