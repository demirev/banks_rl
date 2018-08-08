import math, random
import numpy as np

import torch
import torch.nn as nn
import torch.optim as optim
import torch.autograd as autograd 
import torch.nn.functional as F

USE_CUDA = False#torch.cuda.is_available()
Variable = lambda *args, **kwargs: autograd.Variable(*args, **kwargs).cuda() if USE_CUDA else autograd.Variable(*args, **kwargs)

from collections import deque

class ReplayBuffer(object):
    def __init__(self, capacity):
        self.buffer = deque(maxlen=capacity)
    
    def push(self, state, action, reward, next_state):
        state      = np.expand_dims(state, 0)
        next_state = np.expand_dims(next_state, 0)
            
        self.buffer.append((state, action, reward, next_state))
    
    def sample(self, batch_size):
        state, action, reward, next_state = zip(*random.sample(self.buffer, batch_size))
        return np.concatenate(state), action, reward, np.concatenate(next_state)
    
    def getLen(self):
        return len(self.buffer)
        
class DQN(nn.Module):
    def __init__(self, num_inputs, num_actions, num_hidden = 128, num_hidden1 = 128):
        super(DQN, self).__init__()
        
        self.layers = nn.Sequential(
            nn.BatchNorm1d(num_inputs),
            nn.Linear(num_inputs, num_hidden),
            nn.ReLU(),
            nn.BatchNorm1d(num_hidden),
            nn.Linear(num_hidden, num_hidden1),
            nn.ReLU(),
            nn.BatchNorm1d(num_hidden1),
            nn.Linear(num_hidden1, num_actions)
        )
        
        self.outsize = num_actions
        
    def forward(self, x):
        return self.layers(x)
    
    def act(self, state, epsilon):
        if random.random() > epsilon:
            state   = Variable(torch.FloatTensor(state).unsqueeze(0), volatile=True)
            q_value = self.forward(state)
            action  = q_value.max(1)[1].data[0].item()
        else:
            action = random.randrange(self.outsize)
        return action
        
def compute_td_loss(batch_size, replay_buffer, current_model, target_model, optimizer, beta):
    state, action, reward, next_state = replay_buffer.sample(batch_size)

    state      = Variable(torch.FloatTensor(np.float32(state)))
    next_state = Variable(torch.FloatTensor(np.float32(next_state)))
    action     = Variable(torch.LongTensor(action))
    reward     = Variable(torch.FloatTensor(reward))

    q_values      = current_model(state)
    next_q_values = current_model(next_state)
    next_q_state_values = target_model(next_state)
    
    q_value       = q_values.gather(1, action.unsqueeze(1)).squeeze(1) 
    next_q_value = next_q_state_values.gather(1, torch.max(next_q_values, 1)[1].unsqueeze(1)).squeeze(1)
    expected_q_value = reward + beta * next_q_value
    
    loss = (q_value - Variable(expected_q_value.data)).pow(2).mean()
        
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    
    return loss
    
def update_target(current_model, target_model):
    target_model.load_state_dict(current_model.state_dict())
