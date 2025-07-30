# Support Classes for the LSTM imputation
# Includes the classifier model and the dataset

class LSTMForecaster(nn.Module):

    def __init__(self, n_features, n_hidden, n_outputs, sequence_len, n_lstm_layers=1, bidirectional = True, n_deep_layers=10, use_cuda=False, dropout=0.2):
        '''
        n_features: number of input features (1 for univariate forecasting)
        n_hidden: number of neurons in each hidden layer
        n_outputs: number of outputs to predict for each training example
        n_deep_layers: number of hidden dense layers after the lstm layer
        sequence_len: number of steps to look back at for prediction
        dropout: float (0 < dropout < 1) dropout ratio between dense layers
        '''
        super().__init__()

        self.n_lstm_layers = n_lstm_layers
        self.nhid = n_hidden
        self.use_cuda = use_cuda # set option for device selection
        self.bidirectional = bidirectional

        # LSTM Layer
        self.lstm = nn.LSTM(n_features,
                            n_hidden,
                            num_layers = n_lstm_layers,
                            batch_first = True,
                            bidirectional = bidirectional) # As we have transformed our data in this way

        # first dense after lstm
        hidsize = n_hidden * sequence_len * 2 if self.bidirectional else n_hidden * sequence_len
        self.fc1 = nn.Linear(hidsize, n_hidden) 
        # Dropout layer 
        self.dropout = nn.Dropout(p=dropout)

        # Create fully connected layers (n_hidden x n_deep_layers)
        dnn_layers = []
        for i in range(n_deep_layers):
          # Last layer (n_hidden x n_outputs)
          if i == n_deep_layers - 1:
            dnn_layers.append(nn.ReLU())
            dnn_layers.append(nn.Linear(n_hidden, n_outputs))
          # All other layers (n_hidden x n_hidden) with dropout option
          else:
            dnn_layers.append(nn.ReLU())
            dnn_layers.append(nn.Linear(n_hidden, n_hidden))
            if dropout:
                dnn_layers.append(nn.Dropout(p=dropout))
        # compile DNN layers
        self.dnn = nn.Sequential(*dnn_layers)

    def forward(self, x):

        # Initialize hidden state
        hidsize = self.n_lstm_layers * 2 if self.bidirectional else self.n_lstm_layers
        hidden_state = torch.zeros(hidsize, x.shape[0], self.nhid)
        cell_state = torch.zeros(hidsize, x.shape[0], self.nhid)

        # move hidden state to device
        if self.use_cuda:
            hidden_state = hidden_state.to(device)
            cell_state = cell_state.to(device)

        self.hidden = (hidden_state, cell_state)

        # Forward Pass
        x, h = self.lstm(x, self.hidden) # LSTM
        x = self.dropout(x.contiguous().view(x.shape[0], -1)) # Flatten lstm out 
        x = self.fc1(x) # First Dense
        return self.dnn(x) # Pass forward through fully connected DNN.



class SequenceDataset(Dataset):
    def __init__(self, data_dict: dict, data_type: str = 'Training'):
        '''
        data_dict: Dictionary with different data sequences. Keys are based on the numeric index of the master dataset.
        '''
        
        self.data = data_dict
        self.data_type = data_type
    
    def __len__(self):
        return len(self.data)
    
    def __getitem__(self, idx):
        sample = self.data[idx]
        return sample['x_past'], sample['y_past'], sample['x_future'], sample['y_target']    

class SequenceMaster():

    def __init__(self, df: pd.DataFrame, tw: int, pw: int, y_column: str, validation_column: str = None, x_columns = []):
        '''
        df: Pandas DataFrame of the univariate time-series and covariates
        tw: Training Window - Integer defining how many steps to look back
        pw: Prediction Window - Integer defining how many steps to predict
        y_column: Name of the column (str) for prediction
        x_columns: List with names of other covariates
        '''        
        
        self.brute_data = df.sort_index()
        self.y_column = y_column
        self.x_columns = x_columns
        self.tw, self.pw = tw, pw
        self.validation_column = validation_column
        
        ## Get data sequences for covariates
        if len(self.x_columns) == 0:   
            self.x_columns = list(dataframe.columns)
            self.x_columns.remove(y_column)       
        
        train_dict, val_dict = self.generate_sequences(return_validation = validation_column is not None)
        
        # Create Datasets
        self.train = SequenceDataset(train_dict, 'Training')
        self.validation = SequenceDataset(val_dict, 'Validation')

    
    def __len__(self):
        return len(self.train) + len(self.validation)
    
    def generate_sequences(self, return_validation = False):
        '''
        returns: dictionary of sequences and targets for all sequences
        '''
        train, validation =  {}, {} # Store results into a dictionary
        L = len(self.brute_data)
        skipped_count = 0
        l, m = 0, 0
        for i in range(L - self.tw):
            target_row_idx = i + self.tw
            if target_row_idx > L:    break # Stop if prediction windows is larger than future data
            if target_row_idx + self.pw >= L:    break
            
            # Determine Data Type
            data_type = 'train'
            if return_validation:
                if self.brute_data.iloc[target_row_idx][self.validation_column]:
                    data_type = 'validation'   
            
            # Check if Prediction Window for training data falls within Validation Frame
            if return_validation and (data_type != 'validation'):
                if self.brute_data.iloc[target_row_idx + self.pw][self.validation_column]:
                    skipped_count += 1
                    continue # If the prediction window includes validation data, do not add to training
                    
            # Get Future data
             ## Get sequences for target values
            y_target = self.brute_data[target_row_idx : target_row_idx + self.pw][self.y_column].values
             ## Get data sequences for covariates
            x_future = self.brute_data[target_row_idx : target_row_idx + self.pw][self.x_columns].values
            
            
            ### Get Past Data
             ## Get data sequences for target values
            train_y = self.brute_data[i : target_row_idx][self.y_column].values
             ## Get data sequences for covariates
            train_x = self.brute_data[i : target_row_idx][self.x_columns].values

            ### Get values right after the current sequence
            if data_type == 'train':
                train[l] = {'x_past': np.float32(train_x), 'y_past': np.float32(train_y), 
                            'x_future': np.float32(x_future), 'y_target': np.float32(y_target)}
                l += 1
            elif data_type == 'validation':
                validation[m] = {'x_past': np.float32(train_x), 'y_past': np.float32(train_y), 
                                 'x_future': np.float32(x_future), 'y_target': np.float32(y_target)}
                m += 1
            
        
        print(f'Skipped {skipped_count} tranining samples as they included validation data')
        return train, validation
    