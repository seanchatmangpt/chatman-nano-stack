#!/usr/bin/env python3
"""
CNS Forex Strategy Module - AOT Optimized
Integrates with existing Python AOT infrastructure for ultra-fast strategy execution
"""

import numpy as np
import numba
from numba import jit, cuda, types
from typing import Tuple, Dict, List
import ctypes
from ctypes import Structure, c_uint32, c_int64, c_double, c_uint8

# C struct definitions for integration
class ForexTick(Structure):
    _fields_ = [
        ("timestamp", c_uint32),
        ("pair_id", c_uint32),
        ("bid", c_int64),
        ("ask", c_int64),
        ("volume", c_uint32),
        ("flags", c_uint8),
    ]

class StrategySignal(Structure):
    _fields_ = [
        ("pair_id", c_uint32),
        ("signal_strength", c_int64),  # -100 to +100
        ("action", c_uint8),           # BUY=1, SELL=2, CLOSE=3
        ("confidence", c_uint8),       # 0-100%
        ("strategy_id", c_uint32),
        ("risk_reward_ratio", c_double),
    ]

# Strategy constants
BUY_SIGNAL = 1
SELL_SIGNAL = 2
CLOSE_SIGNAL = 3

# Pair IDs from C header
CURRENCY_PAIRS = {
    "EURUSD": 0x00, "GBPUSD": 0x01, "USDJPY": 0x02, "USDCHF": 0x03,
    "AUDUSD": 0x04, "USDCAD": 0x05, "NZDUSD": 0x06, "EURGBP": 0x07,
    "EURJPY": 0x08, "GBPJPY": 0x09, "CHFJPY": 0x0A, "EURAUD": 0x0B,
}

@numba.jit(nopython=True, cache=True, fastmath=True)
def calculate_sma(prices: np.ndarray, period: int) -> float:
    """Ultra-fast SMA calculation using Numba AOT"""
    if len(prices) < period:
        return 0.0
    return np.mean(prices[-period:])

@numba.jit(nopython=True, cache=True, fastmath=True)
def calculate_ema(prices: np.ndarray, period: int, alpha: float = 0.0) -> float:
    """Ultra-fast EMA calculation using Numba AOT"""
    if alpha == 0.0:
        alpha = 2.0 / (period + 1.0)
    
    if len(prices) == 0:
        return 0.0
    
    ema = prices[0]
    for i in range(1, len(prices)):
        ema = alpha * prices[i] + (1.0 - alpha) * ema
    
    return ema

@numba.jit(nopython=True, cache=True, fastmath=True)
def calculate_rsi(prices: np.ndarray, period: int = 14) -> float:
    """Ultra-fast RSI calculation using Numba AOT"""
    if len(prices) < period + 1:
        return 50.0
    
    deltas = np.diff(prices)
    gains = np.where(deltas > 0, deltas, 0.0)
    losses = np.where(deltas < 0, -deltas, 0.0)
    
    avg_gain = np.mean(gains[-period:])
    avg_loss = np.mean(losses[-period:])
    
    if avg_loss == 0.0:
        return 100.0
    
    rs = avg_gain / avg_loss
    rsi = 100.0 - (100.0 / (1.0 + rs))
    
    return rsi

@numba.jit(nopython=True, cache=True, fastmath=True)
def calculate_bollinger_bands(prices: np.ndarray, period: int = 20, std_dev: float = 2.0) -> Tuple[float, float, float]:
    """Ultra-fast Bollinger Bands calculation"""
    if len(prices) < period:
        mid = prices[-1] if len(prices) > 0 else 0.0
        return mid, mid, mid
    
    recent_prices = prices[-period:]
    mid = np.mean(recent_prices)
    std = np.std(recent_prices)
    
    upper = mid + (std_dev * std)
    lower = mid - (std_dev * std)
    
    return upper, mid, lower

@numba.jit(nopython=True, cache=True, fastmath=True)
def calculate_macd(prices: np.ndarray, fast: int = 12, slow: int = 26, signal: int = 9) -> Tuple[float, float, float]:
    """Ultra-fast MACD calculation"""
    if len(prices) < slow:
        return 0.0, 0.0, 0.0
    
    ema_fast = calculate_ema(prices, fast)
    ema_slow = calculate_ema(prices, slow)
    macd_line = ema_fast - ema_slow
    
    # Simplified signal line calculation
    signal_line = macd_line * 0.2  # Approximation for speed
    histogram = macd_line - signal_line
    
    return macd_line, signal_line, histogram

class ForexStrategy:
    """Base strategy class with AOT optimization"""
    
    def __init__(self, pair_id: int, strategy_id: int):
        self.pair_id = pair_id
        self.strategy_id = strategy_id
        self.price_history = np.zeros(1000, dtype=np.float64)  # Ring buffer
        self.price_index = 0
        self.position = 0  # Current position: 1=long, -1=short, 0=flat
        
    def update_price(self, price: float):
        """Update price history (ring buffer)"""
        self.price_history[self.price_index] = price
        self.price_index = (self.price_index + 1) % len(self.price_history)
    
    def get_recent_prices(self, count: int) -> np.ndarray:
        """Get recent prices for calculations"""
        if count > len(self.price_history):
            count = len(self.price_history)
        
        start_idx = (self.price_index - count) % len(self.price_history)
        if start_idx + count <= len(self.price_history):
            return self.price_history[start_idx:start_idx + count]
        else:
            # Handle wrap-around
            part1 = self.price_history[start_idx:]
            part2 = self.price_history[:count - len(part1)]
            return np.concatenate([part1, part2])

class MACrossoverStrategy(ForexStrategy):
    """Moving Average Crossover Strategy - AOT Optimized"""
    
    def __init__(self, pair_id: int, fast_period: int = 10, slow_period: int = 21):
        super().__init__(pair_id, 1001)  # Strategy ID
        self.fast_period = fast_period
        self.slow_period = slow_period
        
    @numba.jit(forceobj=True)  # Allow object mode for class methods
    def generate_signal(self, current_price: float) -> StrategySignal:
        """Generate trading signal based on MA crossover"""
        self.update_price(current_price)
        
        prices = self.get_recent_prices(self.slow_period + 5)
        
        if len(prices) < self.slow_period:
            return self._create_signal(0, 0, 0.0)
        
        fast_ma = calculate_sma(prices, self.fast_period)
        slow_ma = calculate_sma(prices, self.slow_period)
        
        # Previous values for crossover detection
        prev_prices = prices[:-1]
        prev_fast_ma = calculate_sma(prev_prices, self.fast_period)
        prev_slow_ma = calculate_sma(prev_prices, self.slow_period)
        
        signal_strength = 0
        action = 0
        confidence = 0.0
        
        # Bullish crossover
        if fast_ma > slow_ma and prev_fast_ma <= prev_slow_ma:
            signal_strength = 75
            action = BUY_SIGNAL
            confidence = 80.0
            self.position = 1
        # Bearish crossover
        elif fast_ma < slow_ma and prev_fast_ma >= prev_slow_ma:
            signal_strength = -75
            action = SELL_SIGNAL
            confidence = 80.0
            self.position = -1
        # Close signal if moving against position
        elif self.position == 1 and fast_ma < slow_ma * 0.995:  # 0.5% buffer
            signal_strength = 0
            action = CLOSE_SIGNAL
            confidence = 70.0
            self.position = 0
        elif self.position == -1 and fast_ma > slow_ma * 1.005:  # 0.5% buffer
            signal_strength = 0
            action = CLOSE_SIGNAL
            confidence = 70.0
            self.position = 0
        
        return self._create_signal(signal_strength, action, confidence)
    
    def _create_signal(self, strength: int, action: int, confidence: float) -> StrategySignal:
        """Create strategy signal structure"""
        signal = StrategySignal()
        signal.pair_id = self.pair_id
        signal.signal_strength = strength
        signal.action = action
        signal.confidence = int(confidence)
        signal.strategy_id = self.strategy_id
        signal.risk_reward_ratio = 2.0  # 2:1 risk/reward
        return signal

class RSIStrategy(ForexStrategy):
    """RSI Overbought/Oversold Strategy - AOT Optimized"""
    
    def __init__(self, pair_id: int, rsi_period: int = 14, overbought: float = 70.0, oversold: float = 30.0):
        super().__init__(pair_id, 1002)
        self.rsi_period = rsi_period
        self.overbought = overbought
        self.oversold = oversold
    
    def generate_signal(self, current_price: float) -> StrategySignal:
        """Generate signal based on RSI levels"""
        self.update_price(current_price)
        
        prices = self.get_recent_prices(self.rsi_period + 10)
        
        if len(prices) < self.rsi_period + 1:
            return self._create_signal(0, 0, 0.0)
        
        rsi = calculate_rsi(prices, self.rsi_period)
        
        signal_strength = 0
        action = 0
        confidence = 0.0
        
        if rsi < self.oversold and self.position != 1:
            # Oversold - buy signal
            signal_strength = int((self.oversold - rsi) * 2)  # Stronger signal when more oversold
            action = BUY_SIGNAL
            confidence = min(90.0, 50.0 + (self.oversold - rsi))
            self.position = 1
        elif rsi > self.overbought and self.position != -1:
            # Overbought - sell signal
            signal_strength = -int((rsi - self.overbought) * 2)
            action = SELL_SIGNAL
            confidence = min(90.0, 50.0 + (rsi - self.overbought))
            self.position = -1
        elif 40 < rsi < 60 and self.position != 0:
            # RSI back to neutral - close position
            signal_strength = 0
            action = CLOSE_SIGNAL
            confidence = 75.0
            self.position = 0
        
        return self._create_signal(signal_strength, action, confidence)
    
    def _create_signal(self, strength: int, action: int, confidence: float) -> StrategySignal:
        signal = StrategySignal()
        signal.pair_id = self.pair_id
        signal.signal_strength = strength
        signal.action = action
        signal.confidence = int(confidence)
        signal.strategy_id = self.strategy_id
        signal.risk_reward_ratio = 1.5
        return signal

class MultiIndicatorStrategy(ForexStrategy):
    """Advanced strategy combining multiple indicators - AOT Optimized"""
    
    def __init__(self, pair_id: int):
        super().__init__(pair_id, 1003)
        
    def generate_signal(self, current_price: float) -> StrategySignal:
        """Generate signal based on multiple indicators"""
        self.update_price(current_price)
        
        prices = self.get_recent_prices(50)
        
        if len(prices) < 26:  # Need enough data for MACD
            return self._create_signal(0, 0, 0.0)
        
        # Calculate all indicators
        rsi = calculate_rsi(prices, 14)
        macd, signal_line, histogram = calculate_macd(prices)
        upper_bb, mid_bb, lower_bb = calculate_bollinger_bands(prices, 20, 2.0)
        
        # Scoring system
        score = 0
        confidence_factors = []
        
        # RSI signals
        if rsi < 30:
            score += 2  # Strong buy
            confidence_factors.append(30 - rsi)
        elif rsi < 40:
            score += 1  # Weak buy
            confidence_factors.append(10)
        elif rsi > 70:
            score -= 2  # Strong sell
            confidence_factors.append(rsi - 70)
        elif rsi > 60:
            score -= 1  # Weak sell
            confidence_factors.append(10)
        
        # MACD signals
        if macd > signal_line and histogram > 0:
            score += 1  # Buy signal
            confidence_factors.append(15)
        elif macd < signal_line and histogram < 0:
            score -= 1  # Sell signal
            confidence_factors.append(15)
        
        # Bollinger Bands signals
        current_price_float = float(current_price)
        if current_price_float < lower_bb:
            score += 1  # Oversold
            confidence_factors.append(20)
        elif current_price_float > upper_bb:
            score -= 1  # Overbought
            confidence_factors.append(20)
        
        # Generate final signal
        signal_strength = score * 20  # Scale to -100 to +100 range
        signal_strength = max(-100, min(100, signal_strength))  # Clamp
        
        if score >= 2:
            action = BUY_SIGNAL
        elif score <= -2:
            action = SELL_SIGNAL
        elif abs(score) <= 1 and self.position != 0:
            action = CLOSE_SIGNAL
            signal_strength = 0
        else:
            action = 0
        
        # Calculate confidence
        confidence = min(95.0, sum(confidence_factors) + 30)
        
        return self._create_signal(signal_strength, action, confidence)
    
    def _create_signal(self, strength: int, action: int, confidence: float) -> StrategySignal:
        signal = StrategySignal()
        signal.pair_id = self.pair_id
        signal.signal_strength = strength
        signal.action = action
        signal.confidence = int(confidence)
        signal.strategy_id = self.strategy_id
        signal.risk_reward_ratio = 2.5
        return signal

# Strategy factory for C integration
class StrategyManager:
    """Manages multiple strategies for different currency pairs"""
    
    def __init__(self):
        self.strategies: Dict[int, List[ForexStrategy]] = {}
        
        # Initialize strategies for major pairs
        major_pairs = [CURRENCY_PAIRS["EURUSD"], CURRENCY_PAIRS["GBPUSD"], 
                      CURRENCY_PAIRS["USDJPY"], CURRENCY_PAIRS["USDCHF"]]
        
        for pair_id in major_pairs:
            self.strategies[pair_id] = [
                MACrossoverStrategy(pair_id, 10, 21),
                RSIStrategy(pair_id, 14, 75, 25),  # Slightly wider RSI bands
                MultiIndicatorStrategy(pair_id)
            ]
    
    def process_tick(self, pair_id: int, price: float) -> List[StrategySignal]:
        """Process tick and generate signals from all strategies for the pair"""
        if pair_id not in self.strategies:
            return []
        
        signals = []
        for strategy in self.strategies[pair_id]:
            signal = strategy.generate_signal(price)
            if signal.action != 0 and signal.confidence > 60:  # Only high-confidence signals
                signals.append(signal)
        
        return signals

# C interface functions (will be called from C via ctypes)
_strategy_manager = StrategyManager()

def c_process_forex_tick(pair_id: int, price: float) -> int:
    """C-callable function to process forex tick"""
    try:
        signals = _strategy_manager.process_tick(pair_id, price)
        return len(signals)
    except Exception:
        return 0

def c_get_strategy_signals(pair_id: int, price: float, signals_array, max_signals: int) -> int:
    """C-callable function to get strategy signals"""
    try:
        signals = _strategy_manager.process_tick(pair_id, price)
        count = min(len(signals), max_signals)
        
        # Copy signals to C array (ctypes)
        for i in range(count):
            signals_array[i] = signals[i]
        
        return count
    except Exception:
        return 0

# AOT compilation of critical functions
if __name__ == "__main__":
    # Pre-compile functions for production use
    print("Compiling strategy functions with Numba AOT...")
    
    # Dummy data to trigger compilation
    dummy_prices = np.random.random(100)
    
    calculate_sma(dummy_prices, 20)
    calculate_ema(dummy_prices, 20)
    calculate_rsi(dummy_prices, 14)
    calculate_bollinger_bands(dummy_prices, 20, 2.0)
    calculate_macd(dummy_prices, 12, 26, 9)
    
    print("AOT compilation complete. Functions are optimized for production.")