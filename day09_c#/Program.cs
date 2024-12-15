using System.IO;

namespace Solution {
  
class Program {
  static void Main(string[] args) {
    string data = ReadInput("input.txt");
    Console.WriteLine("Part 1:");
    SolvePart1(data);
    Console.WriteLine("Part 2:");
    SolvePart2(data);
  }

  static string ReadInput(string filepath) {
    StreamReader sr = new StreamReader(filepath);
    string line = sr.ReadLine();
    // not the best solution, but the compiler wouldn't shut up about nullable
    // variables
    return line != null ? line : "";
  }

  static void SolvePart1(string data) {
    IntervalDeque storageUnpacked = EvalStorage(data);
    IntervalDeque storagePacked = PackStorage(storageUnpacked);
    long checksum = CalculateChecksum(storagePacked);

    Console.WriteLine(checksum);
  }

  static void SolvePart2(string data) {
    IntervalDeque storage = EvalStorage(data);
    storage.PackFiles();
    long checksum = CalculateChecksum(storage);

    Console.WriteLine(checksum);
  }

  static IntervalDeque EvalStorage(string data) {
    IntervalDeque unpacked = new IntervalDeque();
    int id = 0;

    for (int i = 0; i < data.Length-1; i += 2) {
      unpacked.Push(new Interval(id, data[i] ^ 48));
      unpacked.Push(new Interval(-1, data[i+1] ^ 48));
      id++;
    }
    unpacked.Push(new Interval(id, data[data.Length - 1] ^ 48));

    return unpacked;
  }

  static IntervalDeque PackStorage(IntervalDeque unpacked) {
    IntervalDeque packed = new IntervalDeque();
    Interval filler = unpacked.Pop();
    unpacked.Pop(); // remove gap before filler

    for(;;) {
      Interval interval = unpacked.Dequeue();
      Interval gap = unpacked.Dequeue();
      packed.Push(interval);
      // in case we're at the end of the list
      if (gap == null) {
        break;
      }

      while (gap.width > 0 || unpacked.Empty()) {
        int width = Math.Min(filler.width, gap.width);
        filler.width -= width;
        gap.width -= width;
        packed.Push(new Interval(filler.id, width));

        // get a new interval to fill the empty space with if the current filler
        // interval is empty e.g. has zero width left
        if (filler.width == 0) {
          filler = unpacked.Pop();
          unpacked.Pop(); // remove gap before filler
        }
      }

      // in case there's Empty space left over and there's still storage to left
      if (gap.width > 0 && filler.width > 0) {
        filler.width = 0;
        packed.Push(filler);
      }
    }

    if (filler.width > 0) {
      packed.Push(filler);
    }

    return packed;
  }

  static long CalculateChecksum(IntervalDeque intervals) {
    int position = 0;
    long checksum = 0;

    Interval interval = intervals.Dequeue();
    while (interval != null) {
      if (interval.id == -1) {
        position += interval.width;
        interval = intervals.Dequeue();
        continue;
      }
      while (interval.width > 0) {
        checksum += position * interval.id;
        position++;
        interval.width--;
      }
      interval = intervals.Dequeue();
    }

    return checksum;
  }
}

class Interval {
  public int id;
  public int width;
  public Interval next;
  public Interval prev;

  public Interval(int id, int width) {
    this.id = id;
    this.width = width;
    next = null;
    prev = null;
  }
}

class IntervalDeque {
  private Interval head;
  private Interval tail;

  public Interval Pop() {
    if (head == null && tail == null) {
      return null;
    }

    Interval oldTail = tail;
    if (tail == head) {
      head = null;
      tail = null;
    } else {
      tail = tail.prev;
      tail.next = null;
      oldTail.prev = null;
    }

    return oldTail;
  }

  public void Push(Interval interval) {
    if (tail == null && head == null) {
      tail = interval;
      head = interval;
    } else if (tail == head) {
      tail = interval;
      tail.prev = head;
      head.next = tail;
    } else {
      tail.next = interval;
      interval.prev = tail;
      tail = tail.next;
    }
  }

  public Interval Dequeue() {
    if (head == null && tail == null) {
      return null;
    }

    Interval oldHead = head;
    if (head == tail) {
      head = null;
      tail = null;
    } else {
      head = head.next;
      head.prev = null;
      oldHead.next = null;
    }

    return oldHead;
  }

  public void Enqueue(Interval interval) {
    if (tail == null && head == null) {
      tail = interval;
      head = interval;
    } else if (tail == head) {
      head = interval;
      head.next = tail;
      tail.prev = head;
    } else {
      head.prev = interval;
      interval.next = head;
      head = head.prev;
    }
  }

  public bool Empty() {
    return head == null && tail == null;
  }

  public void PackFiles() {
    int id = tail.id;
    Interval file = tail;
    
    while (id >= 0) {
      Interval curr = head;
      while (file.id != id) {
        file = file.prev;
      }

      while (curr != null && curr != file) {
        curr = curr.next;
        if (curr.id == -1 && curr.width >= file.width) {
          Interval interval = new Interval(file.id, file.width);
          curr.width -= file.width;
          curr.prev.next = interval;
          interval.next = curr;
          interval.prev = curr.prev;
          curr.prev = interval;
          // NOTE: I could optimize here by merging "gap nodes"...
          // but I don't feel like it rn...
          file.id = -1;
          break;
        }
      }
      
      id--;
    }
  }
}

}
