# Support Functions to train LSTM models.

def plot_losses(tr, va):
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots()
    ax.plot(tr, label='train')
    ax.plot(va, label='validation')
    plt.show()

def save_checkpoint(model2save, optimizer2save, scheduler2save, save_path, valid_loss, epoch, step, verbose = True):
    if not os.path.isdir(save_path):
        os.mkdir(save_path)            

    fName = os.path.join(save_path, f'model_checkpoint_{epoch}-{step}.pth')

    state_dict = {'model_state_dict': model2save.state_dict(),
                  'optimizer_state_dict':  optimizer2save.state_dict(),
                  'valid_loss': valid_loss,
                  'epoch': epoch,
                  'step_in_epoch': step}

    if scheduler is not None:
        state_dict['scheduler_state_dict'] = scheduler2save.state_dict()    

    torch.save(state_dict, fName)
    if verbose:
        print(f'    Model saved to ==> {fName}')
    return fName

def load_checkpoint(model2populate, optimizer2populate, checkpoint_name, scheduler2populate = None, device = 'cpu'):
    state_dict = torch.load(checkpoint_name, map_location = device)
    print(f'Model loaded from <== {checkpoint_name}')

    model2populate.load_state_dict(state_dict['model_state_dict'])
    optimizer2populate.load_state_dict(state_dict['optimizer_state_dict'])
    if scheduler2populate is not None:
        scheduler2populate.load_state_dict(state_dict['scheduler_state_dict'])
        
    print(f'''Model stats:
        - Epoch = {state_dict['epoch']}
        - Validation Loss = {state_dict['valid_loss']}
    ''')

def make_predictions_from_dataloader(model, unshuffled_dataloader):
    model.eval()
    predictions, actuals = [], []
    for x_past, y_past, x_future, y_target in unshuffled_dataloader:
        with torch.no_grad():
            x_past = x_past.to(device)
            p = model(x_past)
            predictions.append(p.cpu())
            actuals.append(y_target.squeeze())
    predictions = torch.cat(predictions).numpy()
    actuals = torch.cat(actuals).numpy()
    return predictions.squeeze(), actuals