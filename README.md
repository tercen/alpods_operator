# ALPODs Operator

##### Description
The ALPODs (Algorithmic Population Descriptions) Operator performs cluster classification on cytometry data. It builds a decision tree to identify and classify cell populations based on marker expressions.

##### Usage
Input projection|.
---|---
`row`        | represents the variables (e.g., markers, channels)
`column`     | represents the observations (e.g., cells, events)
`y-axis`     | measurement values

Input parameters|.
---|---
`verbose`| boolean, enables detailed logging, default is `true`
`min_samples`| numeric, minimum number of samples required to split a node, default is `10`
`purity_threshold`| numeric, purity threshold for stopping node splitting, default is `0.95`
`effect_size_threshold`| numeric, effect size threshold for relevant subpopulations, default is `0.5`

Output relations|.
---|---
`cell_type`| character, the identified cell type/cluster
`rule`| character, the classification rule used

##### Details
The operator takes cytometry data as input and performs the following steps:
1. Loads data from Tercen projections
2. Transforms the data into a format suitable for the ALPODs algorithm
3. Builds a decision tree to classify cell populations
4. Extracts classification rules from the decision tree
5. Returns the classified cell types and their associated rules

The ALPODs algorithm builds a decision tree by recursively splitting the data based on marker expressions. It identifies the optimal splits that maximize information gain at each step. The algorithm stops splitting when one of the following criteria is met:
- Maximum depth is reached
- Minimum number of samples is reached
- Node purity exceeds the threshold
- No further informative splits can be found

##### References
- ALPODs: Algorithmic Population Descriptions algorithm

##### See Also
- [Tercen](https://tercen.com)
- [Tercen R Client](https://github.com/tercen/tercenApi)