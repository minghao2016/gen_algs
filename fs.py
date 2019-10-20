class Genetic:
    def __init__(self, npop, pop_size, X_train, X_val, y_train,  y_val):
        self.X = X_train
        self.features = X_train.columns
        self.nfeatures = X_train.shape[1]
        self.npop= npop
        self.pop_size = int(pop_size/2)
        
        self.x_tr = X_train
        self.x_vl = X_val
        self.y_tr = y_train
        self.y_vl = y_val
        
        pass
    
    def generate_population(self):
        pop = []
        for i in range(self.pop_size):
            pop.append([True if i else False for i in np.random.randint(2,size=self.nfeatures)])

        pop = pd.DataFrame(pop, columns=self.features)
        return pop.rename('init_{}'.format)
        
    def evaluate_ind(self, ind):
        cols = X.columns[list(ind)]
        
        # using lgb
        lgb_train = lgb.Dataset(self.x_tr[cols], self.y_tr)
        lgb_eval = lgb.Dataset(self.x_vl[cols], self.y_vl)
        params = {
                'boosting_type': 'gbdt',
                'objective': 'regression',
                'metric': {'mse'},
                'learning_rate': 0.01,
                'feature_fraction': 0.5,
                'bagging_fraction': 0.5,
                'bagging_freq': 5,
                'verbose': 0
                }
        gbm = lgb.train(params,
                    lgb_train,
                    num_boost_round=3000,
                    valid_sets=lgb_eval,
                   early_stopping_rounds=20,
                   verbose_eval = 0)

        y_pred = gbm.predict(self.x_vl[cols], num_iteration=gbm.best_iteration)
        error = mse(y_pred, self.y_vl)
        return error
    
    def evaluate_pop(self, pop):
        
        res = []
        
        for ind in pop.values:
            res.append(self.evaluate_ind(ind))
        
        res = pd.Series(res, index=pop.index)
        
        return res.sort_values(ascending=True)
    
    def crossover(self, mom, dad):
        kid = []
        for i in range(len(mom)):
            kid.append(np.random.choice([mom[i],dad[i]]))
            
        return kid
    
    def make_kids(self,parents):
        kids = []
        for i in range(self.pop_size):
            mom, dad = np.random.choice(parents.index,2)
            mom, dad = list(parents.loc[mom]), list(parents.loc[dad])
            kid = self.crossover(mom, dad)
            kids.append(kid)
            
        return pd.DataFrame(kids, columns=self.features)
        
    
    def main(self):
        
        pop = self.generate_population()
        self.ind_cache = pop
        
        score = self.evaluate_pop(pop)
        self.scoreboard = score
        
        epoch = 1
        while epoch < self.npop:
            
            kids = self.make_kids(pop)
            kids.index = ['gen{}_{}'.format(epoch,i) for i in range(self.pop_size)]
            
            self.ind_cache = self.ind_cache.append(kids)
            
            kids_score = self.evaluate_pop(kids)
            
            self.scoreboard = self.scoreboard.append(kids_score)
            score = score.append(kids_score).sort_values(ascending=True)[:self.pop_size]
            print("Epoch : {}           Highest Score : {}".format(epoch, score[0]))
            
            pop = self.ind_cache.loc[score.index,:]
            
            epoch += 1

            
        return self.features[pop.iloc[0,:]]
