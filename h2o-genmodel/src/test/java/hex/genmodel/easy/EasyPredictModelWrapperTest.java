package hex.genmodel.easy;

import hex.ModelCategory;
import hex.genmodel.GenModel;
import hex.genmodel.MojoModel;
import hex.genmodel.algos.word2vec.WordEmbeddingModel;
import hex.genmodel.easy.exception.PredictUnknownCategoricalLevelException;
import hex.genmodel.easy.prediction.BinomialModelPrediction;
import hex.genmodel.easy.prediction.SortedClassProbability;
import hex.genmodel.easy.prediction.Word2VecPrediction;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class EasyPredictModelWrapperTest {
  private static class MyModel extends GenModel {
    MyModel(String[] names, String[][] domains) {
      super(names, domains);
    }

    @Override
    public int nclasses() {
      return 2;
    }

    @Override
    public boolean isSupervised() {
      return true;
    }

    @Override
    public double[] score0(double[] data, double[] preds) {
      Assert.assertEquals(preds.length, 3);
      preds[0] = 0;
      preds[1] = 1.0;
      preds[2] = 0.0;
      return preds;
    }

    @Override
    public ModelCategory getModelCategory() {
      return ModelCategory.Binomial;
    }

    @Override
    public String getUUID() {
      return null;
    }
  }

  private static MyModel makeModel() {
    String[] names = {
            "C1",
            "C2",
            "RESPONSE"};
    String[][] domains = {
            {"c1level1", "c1level2"},
            {"c2level1", "c2level2", "c2level3"},
            {"NO", "YES"}
    };
    return new MyModel(names, domains);
  }

  @Test
  public void testUnknownCategoricalLevels() throws Exception {
    MyModel rawModel = makeModel();
    EasyPredictModelWrapper m = new EasyPredictModelWrapper(rawModel);

    {
      RowData row = new RowData();
      row.put("C1", "c1level1");
      try {
        m.predictBinomial(row);
      } catch (PredictUnknownCategoricalLevelException e) {
        Assert.fail("Caught exception but should not have");
      }
      ConcurrentHashMap<String, AtomicLong> unknown = m.getUnknownCategoricalLevelsSeenPerColumn();
      long total = 0;
      for (AtomicLong l : unknown.values()) {
        total += l.get();
      }
      Assert.assertEquals(total, 0);
    }

    {
      RowData row = new RowData();
      row.put("C1", "c1level1");
      row.put("C2", "unknownLevel");
      boolean caught = false;
      try {
        m.predictBinomial(row);
      } catch (PredictUnknownCategoricalLevelException e) {
        caught = true;
      }
      Assert.assertEquals(caught, true);
      ConcurrentHashMap<String, AtomicLong> unknown = m.getUnknownCategoricalLevelsSeenPerColumn();
      long total = 0;
      for (AtomicLong l : unknown.values()) {
        total += l.get();
      }
      Assert.assertEquals(total, 0);
    }

    m = new EasyPredictModelWrapper(new EasyPredictModelWrapper.Config()
            .setModel(rawModel)
            .setConvertUnknownCategoricalLevelsToNa(true)
            .setConvertInvalidNumbersToNa(true));

    {
      RowData row0 = new RowData();
      m.predict(row0);
      Assert.assertEquals(m.getTotalUnknownCategoricalLevelsSeen(), 0);

      RowData row1 = new RowData();
      row1.put("C1", "c1level1");
      row1.put("C2", "unknownLevel");
      m.predictBinomial(row1);
      Assert.assertEquals(m.getTotalUnknownCategoricalLevelsSeen(), 1);

      RowData row2 = new RowData();
      row2.put("C1", "c1level1");
      row2.put("C2", "c2level3");
      m.predictBinomial(row2);
      Assert.assertEquals(m.getTotalUnknownCategoricalLevelsSeen(), 1);

      RowData row3 = new RowData();
      row3.put("C1", "c1level1");
      row3.put("unknownColumn", "unknownLevel");
      m.predictBinomial(row3);
      Assert.assertEquals(m.getTotalUnknownCategoricalLevelsSeen(), 1);

      m.predictBinomial(row1);
      m.predictBinomial(row1);
      Assert.assertEquals(m.getTotalUnknownCategoricalLevelsSeen(), 3);

      RowData row4 = new RowData();
      row4.put("C1", "unknownLevel");
      m.predictBinomial(row4);
      Assert.assertEquals(m.getTotalUnknownCategoricalLevelsSeen(), 4);
      Assert.assertEquals(m.getUnknownCategoricalLevelsSeenPerColumn().get("C1").get(), 1);
      Assert.assertEquals(m.getUnknownCategoricalLevelsSeenPerColumn().get("C2").get(), 3);
    }
  }

  @Test
  public void testSortedClassProbability() throws Exception {
    MyModel rawModel = makeModel();
    EasyPredictModelWrapper m = new EasyPredictModelWrapper(rawModel);

    {
      RowData row = new RowData();
      row.put("C1", "c1level1");
      BinomialModelPrediction p = m.predictBinomial(row);
      SortedClassProbability[] arr = m.sortByDescendingClassProbability(p);
      Assert.assertEquals(arr[0].name, "NO");
      Assert.assertEquals(arr[0].probability, 1.0, 0.001);
      Assert.assertEquals(arr[1].name, "YES");
      Assert.assertEquals(arr[1].probability, 0.0, 0.001);
    }
  }

  @Test
  public void testWordEmbeddingModel() throws Exception {
    MyWordEmbeddingModel rawModel = new MyWordEmbeddingModel();
    EasyPredictModelWrapper m = new EasyPredictModelWrapper(rawModel);

    RowData row = new RowData();
    row.put("C0", -1); // should be ignored
    row.put("C1", "0.9,0.1");
    row.put("C2", "0.1,0.9");
    row.put("C3", "NA");

    Word2VecPrediction p = m.predictWord2Vec(row);

    Assert.assertFalse(p.wordEmbeddings.containsKey("C0"));
    Assert.assertArrayEquals(new float[]{0.9f, 0.1f}, p.wordEmbeddings.get("C1"), 0.0001f);
    Assert.assertArrayEquals(new float[]{0.1f, 0.9f}, p.wordEmbeddings.get("C2"), 0.0001f);
    Assert.assertTrue(p.wordEmbeddings.containsKey("C3"));
    Assert.assertNull(p.wordEmbeddings.get("C3"));
  }

  private static class MyWordEmbeddingModel extends MojoModel implements WordEmbeddingModel {

    public MyWordEmbeddingModel() {
      super(new String[0], new String[0][]);
    }

    @Override
    public int getVecSize() {
      return 2;
    }

    @Override
    public float[] transform0(String word, float[] output) {
      if (word.equals("NA"))
        return null;
      String[] words = word.split(",");
      for (int i = 0; i < words.length; i++)
        output[i] = Float.valueOf(words[i]);
      return output;
    }

    @Override
    public double[] score0(double[] row, double[] preds) {
      throw new IllegalStateException("Should never be called");
    }

    @Override
    public ModelCategory getModelCategory() {
      return ModelCategory.WordEmbedding;
    }
  }

}
